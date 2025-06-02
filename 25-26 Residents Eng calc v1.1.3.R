library(shiny)
library(bslib)
library(bsicons)
library(dplyr)

# Data	  
pay_scales <- data.frame(
  grade = c("FY1", "FY2", "ST1-2", "CT1-2", "ST3-5", "CT3-4", "ST6-8"),
  base_salary = c(36616, 42008, 49909, 49909, 61825, 61825, 70425)
)

pay_premia <- data.frame(
  type = c("Academia", "GP", "Psych Core", "Psych HST (3 years)", "Psych HST (4 years)", "Histopathology", 
           "EM/OMFS (3 years)", "EM/OMFS (4 years)", "EM/OMFS (5 years)", 
           "EM/OMFS (6 years)", "EM/OMFS (7 years)", "EM/OMFS (8 years)"),
  f_premia = c(5216, 10690, 4347, 4347, 3260, 5216, 8693, 6520, 5216, 4347, 3726, 3260)
)

L_weight <- 2162
Parttime_alwn <- 1000

# Constants
#Income Tax
PERSONAL_ALLOWANCE <- 12570
TAPER_START <- 100000
TAPER_END <- 125140

#Eng/Wales/NI Rates
BASIC_RATE <- 0.2
HIGHER_RATE <- 0.4
ADDITIONAL_RATE <- 0.45

#Eng/Wales/NI Thresh
INCOME_THRESH <- 12570
INCOME_BASIC_THRESH <- 50270
INCOME_HIGHER_THRESH <-125140

#Eng/Wales/NI Bands
BASIC_BAND <- INCOME_BASIC_THRESH - INCOME_THRESH #37700
HIGHER_BAND <- INCOME_HIGHER_THRESH - INCOME_BASIC_THRESH #74870

#Income Tax - Scotland 
#Scottish Income Tax for 2025/26
#Scot Rates
SCOT_STARTER_RATE <- 0.19
SCOT_BASIC_RATE <- 0.20
SCOT_INTERMEDIATE_RATE <- 0.21
SCOT_HIGHER_RATE <- 0.42
SCOT_ADVANCED_RATE <- 0.45
SCOT_TOP_RATE <- 0.48

#Scot Thresh
SCOT_INCOME_THRESH <- 12570
SCOT_STARTER_THRESH <- 15397     # up to this: Starter Rate
SCOT_BASIC_THRESH <- 27491     # up to this: Basic Rate
SCOT_INTERMEDIATE_THRESH <- 43662 # up to this: Intermediate Rate
SCOT_HIGHER_THRESH <- 75000    # up to this: Higher Rate
SCOT_ADVCANCED_THRESH <- 125140    # up to this: Advanced Rate
# above this: Top Rate
#Soct Bands
SCOT_STARTER_BAND <- SCOT_STARTER_THRESH - SCOT_INCOME_THRESH #2827
SCOT_BASIC_BAND <- SCOT_BASIC_THRESH - SCOT_STARTER_THRESH #12094
SCOT_INTERMEDIATE_BAND <- SCOT_INTERMEDIATE_THRESH - SCOT_BASIC_THRESH #16171
SCOT_HIGHER_BAND <- SCOT_HIGHER_THRESH - SCOT_INTERMEDIATE_THRESH #31338
SCOT_ADVANCED_BAND <- SCOT_ADVCANCED_THRESH - SCOT_HIGHER_THRESH #50140

#NI
NI_THRESHOLD <- 12570
NI_BASIC_LIMIT <- 50270
NI_RATE1 <- 0.08
NI_RATE2 <- 0.02

#Student Loans
P1_THRESH <-26065
P2_THRESH <-28470
P4_THRESH <-32745
P5_THRESH <-25000
PG_THRESH <-21000

# UI	
ui <- bslib::page_fluid(
  titlePanel("Resident Doctor Pay Uplift Calculator - England 25/26 v1.1.3"),
  sidebarLayout(
    sidebarPanel(
      width = 4,
      h6(strong("Select Grade")),
      radioButtons("grade", label = NULL, choices = pay_scales$grade),
	  
	  tagList(
		h6(strong("LTFT Options")),
		p("If you’re working less than full time (e.g. 80%), tick the box and fill in the fields below to adjust your uplift and weekend frequency correctly. Please be aware that LTFT calculations may be inaccurate",
			class = "text-muted", style = "margin-top: -8px; margin-bottom: 8px; font-size: 0.9em;")
		),
      checkboxInput("is_ltft", "Are you less than full time?", value = FALSE),
      conditionalPanel(
        condition = "input.is_ltft == true",
        numericInput("ltft_percentageA", "LTFT Percentage (%) (100 if full time)", value = 100, min = 0, max = 100, step = 0.5),
        numericInput("ltft_hoursA", "LTFT Total Hours (use 40 if Full time)", value = 40, min = 0, max = 56),
		uiOutput("ltft_hint_text")  #Tells the user what the calculator thinks their LTFT percent should be
      ),
	  
      h6(strong("Gross Income")),
		checkboxInput("know_gross_income", "Do you know your gross income (excluding locums)?", value = TRUE),
			conditionalPanel(
				condition = "input.know_gross_income == true",
				numericInput("gross_payA", "Total Pay (£)", value = 36616, min = 0)
			),

			conditionalPanel(
				condition = "input.know_gross_income == false",
					tagList(
					numericInput("non_enhanced_hours", "Average total weekly hours", value = 40, min = 0, max = 56, step = 0.25),
					sliderInput("enhanced_hours", label = NULL,
								value = 8, min = 0, step = 0.25, max = 56),
					
					numericInput("weekend_freq", "If you work weekends, what is your avergae frequency? (e.g. 1 in 6 = enter 6). This can be a decimal number. Choose 9 or higher if you don't work weekends. If you only work one of Saturday or Sunday, it is still a full weekend.", value = 6, min = 1),
    
						# Nested panel: only show if LTFT
						conditionalPanel(
							condition = "input.is_ltft == true",
							numericInput("ft_weekend_freq", "What is the Full-time Weekend Frequency for your rota? (e.g. 1 in 5 = enter 5).", value = 5, min = 1)
						),
					checkboxInput("is_on_call_allowance", "Do you get an on-call allowance?", value = FALSE)
    
					
					)
			),
      
      h6(strong("Tax Region")),
      checkboxInput("is_scotland", "Are you a Scottish taxpayer?", value = FALSE),
      
      uiOutput("premia_selector"),
      
      h6(strong("London Weighting")),
      checkboxInput("is_London_weight", "Do you get London Weighting?", value = FALSE),
      conditionalPanel(
        condition = "input.is_London_weight == true",
        selectInput("london_weighting", "London Weighting Band:",
                    choices = c("Fringe", "Outer", "Inner"),
                    selected = "Inner")
      ),
      h6(strong("Pension Options")),
      checkboxInput("is_under_StatePensionAge", "Are you under state pension age?", value = TRUE),
      conditionalPanel(
        condition = "input.is_under_StatePensionAge == true",
        checkboxInput("is_opt_in_pension", "Are you opted in to the pension?", value = TRUE)
      ),
            
      h6(strong("Select Student Loan Plans You Repay")),
      checkboxGroupInput("plans", label = NULL,
                         choices = list(
                           "Plan 1 (pre-2012)" = "plan1",
                           "Plan 2 (post-2012)" = "plan2",
                           "Plan 4 (Scotland)" = "plan4",
                           "Plan 5 (England, 2023+)" = "plan5",
                           "Postgraduate Loan" = "pgloan"
                         )
      ),
      actionButton("calculate", "Calculate Uplift")
    ),
    mainPanel(
      verbatimTextOutput("salary_output"),
	  uiOutput("pension_warning"),
	  plotOutput("barplot_output")
    )
  )
)

server <- function(input, output, session) {
  
  #Flexible premia selector
  output$premia_selector <- renderUI({
    grade <- input$grade
    choices <- if (grade %in% c("FY1", "FY2")) {
		c("Academia")  # only academia allowed
	} else {
		pay_premia$type
	}
    
    tagList(
		div(
		style = "display: flex; align-items: center; gap: 8px;",
		h6(strong("Flexible Pay Premia Type")),
		tags$span(
			bs_icon("info-circle", size = "1.25em", class = "text-primary"),
			title = "You can select up to two options, but if selecting two options, one must be 'Academia'. If nothing is selected, no premia is applied.",
			`data-bs-toggle` = "tooltip",
			`data-bs-placement` = "right",
			style = "cursor: pointer;"
		)
    ),
    checkboxGroupInput(
      "flexible_pay_premia",
      label = NULL,
      choices = choices,
      selected = NULL
    )
  )
})
  

  
	output$ltft_hint_text <- renderUI({
			if (!input$is_ltft || is.null(input$non_enhanced_hours) || is.null(input$enhanced_hours) ||
				is.na(input$non_enhanced_hours) || is.na(input$enhanced_hours)) {
			return(NULL)
		}

		basic_hours <- input$non_enhanced_hours - input$enhanced_hours

		if (basic_hours < 0) {
			return(tags$div(style = "color: red;", "Enhanced hours cannot exceed total hours."))
		}

		calculated_pct <- round((basic_hours / 40) * 100, 1)
		tags$div(
			style = "color: #0d6efd; font-size: 90%; margin-top: -10px;",
			paste0("Hint: Your LTFT % should be approximately ", calculated_pct, "% based on your hours (", basic_hours, " basic hrs/wk).","\n","If you use a percentage that's too different, the result may be highly innacurate.")
		)
	})

	observe({
		total_hours <- input$non_enhanced_hours
		current_enhanced <- input$enhanced_hours
		if (!is.null(total_hours) && !is.na(total_hours) && total_hours >= 0) {
			new_enhanced <- min(current_enhanced, total_hours)
			updateSliderInput(session, "enhanced_hours", max = total_hours, value = new_enhanced)
		}
	})

	observe({
		total_hours <- input$non_enhanced_hours
		if (!is.null(total_hours) && !is.na(total_hours) && total_hours > 0) {
			updateSliderInput(session, "enhanced_hours",
                      max = total_hours,
                      label = paste0("How many of those ", total_hours, " hours are enhanced/OOH?"))
		}
	})


 
  observeEvent(input$calculate, {
    #inputs	
    get_london_weighting <- function(band) {
      switch(band,
             "Fringe" = 149,
             "Outer"  = 527,
             "Inner"  = 2162,
             0)
    }
    
	#select correct base pay
    base_salary <- pay_scales %>% filter(grade == input$grade) %>% pull(base_salary)
    #handle null for pay premia
	selected_premia <- if (is.null(input$flexible_pay_premia)) {
		0
	} else {
		sum(pay_premia$f_premia[pay_premia$type %in% input$flexible_pay_premia], na.rm = TRUE)
	}
	#london weight
    selected_weight <- if (input$is_London_weight) get_london_weighting(input$london_weighting) else 0
    #ltft
	ltft_premia <- if (input$is_ltft) 1000 else 0
	ltft_percentage <- if (input$is_ltft && !is.null(input$ltft_percentageA)) input$ltft_percentageA else 100
	ltft_hours <- if (input$is_ltft && !is.null(input$ltft_hoursA) && input$ltft_hoursA > 0) input$ltft_hoursA else 40
    
    ltft_multiplier <- ltft_percentage / 100
    pension_factor <- min(ltft_hours / 40, 1)
    #adjustments			 
    adj_grade <- base_salary * ltft_multiplier
    adj_premia <- selected_premia * ltft_multiplier
    adj_weight <- selected_weight * ltft_multiplier
	
	# Uplift calcs
	uplifted_grade <- ceiling(base_salary * 1.04 + 750)
	grade_mult <- uplifted_grade / base_salary
				  
    new_adj_grade <- uplifted_grade * ltft_multiplier
    new_premia <- ceiling(selected_premia * 1.04) * ltft_multiplier

	# Initialise values
	enhanced_pay <- 0
	non_enhanced_pay <- adj_grade
	adj_weekend_pay <- 0
	adj_on_call <- 0

	# Default uplifted values (will be overwritten if gross is not known)
	new_enhanced_pay <- enhanced_pay * grade_mult
	new_non_enhanced_pay <- non_enhanced_pay * grade_mult
	new_adj_weekend_pay <- adj_weekend_pay * grade_mult
	new_adj_on_call <- adj_on_call * grade_mult

	# Input validation
	if (is.null(input$non_enhanced_hours) || is.null(input$enhanced_hours) ||
		is.na(input$non_enhanced_hours) || is.na(input$enhanced_hours)) {
	showNotification("Please enter your average total weekly hours and enhanced hours.", type = "error")
	return()
	}
	total_time <- input$non_enhanced_hours	
	enhanced_time <- input$enhanced_hours
	basic_hours <- total_time - enhanced_time

	if (input$know_gross_income) {
		if (is.null(input$gross_payA) || is.na(input$gross_payA)) {
		showNotification("Please enter your gross pay.", type = "error")
		return()
	}
	gross_pay <- input$gross_payA
	} else {

	# Estimate based on rota
	base_hourly <- base_salary / 40 / 52
	base_hourly_new <- uplifted_grade / 40 / 52
	enhanced_multiplier <- 1.37
	non_enhanced_multiplier <- 1.00

	# Weekend frequency validation
	if (is.null(input$weekend_freq) || is.na(input$weekend_freq) || input$weekend_freq < 1) {
		showNotification("Please enter a valid Weekend Frequency (e.g. 5 for 1 in 5).", type = "error")
		return()
	}

	if (input$is_ltft) {
		if (is.null(input$ft_weekend_freq) || is.na(input$ft_weekend_freq) || input$ft_weekend_freq < 1) {
		showNotification("Please enter a valid full-time Weekend Frequency.", type = "error")
		return()
		}
	}

	weekend_freq <- input$weekend_freq
	ft_weekend_freq <- if (input$is_ltft) input$ft_weekend_freq else input$weekend_freq
	if (ft_weekend_freq > weekend_freq){
		ft_weekend_freq <- weekend_freq
	}

	weekend_freq_percent <- ft_weekend_freq / weekend_freq

	weekend_supplement <- function(freq, salary) {
		if (freq <= 2) return(salary * 0.15)
		if (freq <= 3) return(salary * 0.10)
		if (freq <= 4) return(salary * 0.075)
		if (freq <= 5) return(salary * 0.06)
		if (freq <= 6) return(salary * 0.05)
		if (freq <= 7) return(salary * 0.04)
		if (freq <= 8) return(salary * 0.03)
		return(0)
	}

	weekend_pay <- weekend_supplement(ft_weekend_freq, base_salary)
	adj_weekend_pay <- weekend_pay * weekend_freq_percent

	# On-call
	on_call_allowance <- if (input$is_on_call_allowance) base_salary * 0.08 else 0
	adj_on_call <- weekend_freq_percent * on_call_allowance

	# OOH Pay breakdown
	enhanced_pa <- input$enhanced_hours * base_hourly * enhanced_multiplier * 52
	non_enhanced_pa <- basic_hours * base_hourly * non_enhanced_multiplier * 52
	enhanced_pay <- enhanced_pa * (0.37/1.37)
	non_enhanced_pay <- non_enhanced_pa + enhanced_pa * (1/1.37)

	# Gross
	gross_pay <- non_enhanced_pay + enhanced_pay + adj_weekend_pay + adj_on_call + adj_premia + adj_weight + ltft_premia 

	# ✅ Recalculate uplifted values using uplifted grade
	new_enhanced_pa <- input$enhanced_hours * base_hourly_new * enhanced_multiplier * 52
	new_non_enhanced_pa <- basic_hours * base_hourly_new * non_enhanced_multiplier * 52
	new_enhanced_pay <- new_enhanced_pa * (0.37/1.37)
	new_non_enhanced_pay <- new_non_enhanced_pa + new_enhanced_pa * (1/1.37)	
	
	new_adj_weekend_pay <- weekend_supplement(ft_weekend_freq, uplifted_grade) * weekend_freq_percent
	new_on_call_allowance <- if (input$is_on_call_allowance) uplifted_grade * 0.08 else 0
	new_adj_on_call <- weekend_freq_percent * new_on_call_allowance
	}

		#Gross less OOH, and new_gross:
			if (input$know_gross_income) {
			OOH_pay <- gross_pay - (adj_grade + adj_premia + adj_weight + ltft_premia)
			Gross_less_OOH <- gross_pay - OOH_pay
			    new_OOH <- OOH_pay * grade_mult
				new_gross <- new_adj_grade + new_OOH + new_premia + ltft_premia + adj_weight
			} else {OOH_pay <- non_enhanced_pay + enhanced_pay + adj_weekend_pay + adj_on_call - adj_grade #if user has been accurate, adj_grade should equal non_enhanced_pay
					Gross_less_OOH <- gross_pay - OOH_pay
					new_OOH <- new_non_enhanced_pay + new_enhanced_pay + new_adj_weekend_pay + new_adj_on_call - new_adj_grade #if user has been accurate, new_adj_grade should equal new_non_enhanced_pay
					new_gross <- new_non_enhanced_pay + new_enhanced_pay + new_adj_weekend_pay + new_adj_on_call + new_premia + adj_weight + ltft_premia
			} 
			
	#Warn about human error
	
			#If Ltft percentage does not match basic hours
			expected_basic_hours <- ltft_multiplier * 40
			tolerance <- 2  # hours

			if (input$is_ltft && !input$know_gross_income && abs(basic_hours - expected_basic_hours) > tolerance) {
				showNotification("Warning: Your LTFT percentage and basic hours may not match. Please double-check your inputs. Results may be wrong.", type = "warning")
			}
			
			#less than 40hrs, but not LTFT
			if (!input$is_ltft && input$non_enhanced_hours < 40) {
					showNotification("Warning: You do less than 40 hours per week, but you did not tick LTFT. Is this correct?", type = "warning")
				}
			
				
			#input gross pay is less than (estimated minimum gross, using inputs)
			if (gross_pay < Gross_less_OOH) {
					showNotification("Error: The total pay you entered was below the minimum expected - based on your London weighting, LTFT allowance, pay premia and base pay. Please enter the correct gross income", type = "Error")
					return() #this line stops the calculations from proceeding
				}
			
			#input hours are over 48 (with null input check)
			if (!is.null(input$non_enhanced_hours) && !is.na(input$non_enhanced_hours) &&
					input$non_enhanced_hours > 48) {
					showNotification("Warning: The total of the enhanced + non enhanced hours you entered are greater than 48. If you have not opted out of the EWTD your hours should be 48 or less. If you have opted out, your hours should be 56 or less. Are your hours correct?", type = "warning")
				}
			
			#if ltft weekend frequency is higher than ft weekend frequency
			if (input$is_ltft && input$ft_weekend_freq > input$weekend_freq){
				showNotification("Warning: You are LTFT and have stated you do more frequent weekends than your full time colleagues. Unfortunately I do not know the correct way to handle this info, so I am assuming I do not pro-rata this.", type = "warning")
			}
				
			
    
    #Pension opt in/out		   
    pension_opt <- ifelse(!input$is_under_StatePensionAge || !input$is_opt_in_pension, 0, 1)																	  
    pensionable <- (base_salary * pension_factor) + adj_weight
    pension_base <- ifelse(pension_opt < 1, 0, pensionable)										 
	 	
    #New Pension calcs			   
    new_pensionable <- (uplifted_grade * pension_factor) + adj_weight
    new_pension_base <- ifelse(pension_opt < 1, 0, new_pensionable)
    
    #Pension function (pensionable pay is made of base salary and london weighting, adjust for ltft (but ltft OOH are included if below 40))																																																						 
    pension_calc <- function(x) {
      if (x <= 13259) return(x * 0.052)
      if (x <= 27288) return(x * 0.065)
      if (x <= 33247) return(x * 0.083)
      if (x <= 49913) return(x * 0.098)
      if (x <= 63994) return(x * 0.107)
      return(x * 0.125)
    }
    
    pension_ded <- pension_calc(pension_base)
    new_pension_ded <- pension_calc(new_pension_base)
    
    adjGross <- gross_pay - pension_ded
    new_adj_gross <- new_gross - new_pension_ded
    
    # Student loans						  
    plan_data <- list(
      plan1 = list(threshold = P1_THRESH, rate = 0.09),
      plan2 = list(threshold = P2_THRESH, rate = 0.09),
      plan4 = list(threshold = P4_THRESH, rate = 0.09),
      plan5 = list(threshold = P5_THRESH, rate = 0.09),
      pgloan = list(threshold = PG_THRESH, rate = 0.06)
    )
    
    selected_plans <- input$plans
    has_pg <- "pgloan" %in% selected_plans
    undergrad_plans <- setdiff(selected_plans, "pgloan")
    
    loan_repay <- function(net) {
      total <- 0
      if (length(undergrad_plans) > 0) {
        min_plan <- undergrad_plans[which.min(sapply(undergrad_plans, function(p) plan_data[[p]]$threshold))]
        thresh <- plan_data[[min_plan]]$threshold
        total <- total + max(0, net - thresh) * plan_data[[min_plan]]$rate
      }
      if (has_pg) {
        pg_thresh <- max(plan_data$pgloan$threshold, ifelse(length(undergrad_plans) > 0, plan_data[[min_plan]]$threshold, 0))
        total <- total + max(0, net - pg_thresh) * plan_data$pgloan$rate
      }
      return(total)
    }
    
    student_loan_old <- loan_repay(gross_pay)
    student_loan_new <- loan_repay(new_gross)
    
    #Income Tax and NI						   
    #Income Tax
    income_tax <- function(gross, is_scotland = FALSE) {
      pa <- PERSONAL_ALLOWANCE
      if (gross > TAPER_START && gross <= TAPER_END) {
        pa <- max(0, PERSONAL_ALLOWANCE - (gross - TAPER_START) / 2)
      } else if (gross > TAPER_END) {
        pa <- 0
      }
      
      taxable_income <- max(0, gross - pa)
      
      if (!is_scotland) {
        # England/Wales/NI tax bands
        if (taxable_income <= BASIC_BAND) {
          return(taxable_income * BASIC_RATE)
        } else if (taxable_income <= BASIC_BAND + HIGHER_BAND) {
          return(BASIC_BAND * BASIC_RATE +
                   (taxable_income - BASIC_BAND) * HIGHER_RATE)
        } else {
          return(BASIC_BAND * BASIC_RATE +
                   HIGHER_BAND * HIGHER_RATE +
                   (taxable_income - BASIC_BAND - HIGHER_BAND) * ADDITIONAL_RATE)
        }
      }

	  else {
    # Scotland tax bands
    if (taxable_income <= SCOT_STARTER_BAND) {
      return(taxable_income * SCOT_STARTER_RATE)
    } else if (taxable_income <= SCOT_STARTER_BAND + SCOT_BASIC_BAND) {
      return(
        SCOT_STARTER_BAND * SCOT_STARTER_RATE +
        (taxable_income - SCOT_STARTER_BAND) * SCOT_BASIC_RATE
      )
    } else if (taxable_income <= SCOT_STARTER_BAND + SCOT_BASIC_BAND + SCOT_INTERMEDIATE_BAND) {
      return(
        SCOT_STARTER_BAND * SCOT_STARTER_RATE +
        SCOT_BASIC_BAND * SCOT_BASIC_RATE +
        (taxable_income - SCOT_STARTER_BAND - SCOT_BASIC_BAND) * SCOT_INTERMEDIATE_RATE
      )
    } else if (taxable_income <= SCOT_STARTER_BAND + SCOT_BASIC_BAND + SCOT_INTERMEDIATE_BAND + SCOT_HIGHER_BAND) {
      return(
        SCOT_STARTER_BAND * SCOT_STARTER_RATE +
        SCOT_BASIC_BAND * SCOT_BASIC_RATE +
        SCOT_INTERMEDIATE_BAND * SCOT_INTERMEDIATE_RATE +
        (taxable_income - SCOT_STARTER_BAND - SCOT_BASIC_BAND - SCOT_INTERMEDIATE_BAND) * SCOT_HIGHER_RATE
      )
    } else if (taxable_income <= SCOT_STARTER_BAND + SCOT_BASIC_BAND + SCOT_INTERMEDIATE_BAND + SCOT_HIGHER_BAND + SCOT_ADVANCED_BAND) {
      return(
        SCOT_STARTER_BAND * SCOT_STARTER_RATE +
        SCOT_BASIC_BAND * SCOT_BASIC_RATE +
        SCOT_INTERMEDIATE_BAND * SCOT_INTERMEDIATE_RATE +
        SCOT_HIGHER_BAND * SCOT_HIGHER_RATE +
        (taxable_income - SCOT_STARTER_BAND - SCOT_BASIC_BAND - SCOT_INTERMEDIATE_BAND - SCOT_HIGHER_BAND) * SCOT_ADVANCED_RATE
      )
    } else {
      return(
        SCOT_STARTER_BAND * SCOT_STARTER_RATE +
        SCOT_BASIC_BAND * SCOT_BASIC_RATE +
        SCOT_INTERMEDIATE_BAND * SCOT_INTERMEDIATE_RATE +
        SCOT_HIGHER_BAND * SCOT_HIGHER_RATE +
        SCOT_ADVANCED_BAND * SCOT_ADVANCED_RATE +
        (taxable_income - SCOT_STARTER_BAND - SCOT_BASIC_BAND - SCOT_INTERMEDIATE_BAND - SCOT_HIGHER_BAND - SCOT_ADVANCED_BAND) * SCOT_TOP_RATE
      )
    }
  }
}
    #NI
    ni_calc <- function(gross) {
      if (gross <= NI_THRESHOLD) {
        return(0)
      } else if (gross <= NI_BASIC_LIMIT) {
        return((gross - NI_THRESHOLD) * NI_RATE1)
      } else {
        basic_ni_band <- NI_BASIC_LIMIT - NI_THRESHOLD
        upper_ni_band <- gross - NI_BASIC_LIMIT
        return(basic_ni_band * NI_RATE1 + upper_ni_band * NI_RATE2)
      }
    }
    
    scotland_flag <- isTRUE(input$is_scotland)
    income_tax_old <- income_tax(adjGross, scotland_flag)
    income_tax_new <- income_tax(new_adj_gross, scotland_flag)
    
    ni_old <- ifelse(input$is_under_StatePensionAge, ni_calc(adjGross), 0)
    ni_new <- ifelse(input$is_under_StatePensionAge, ni_calc(new_adj_gross), 0)
    
    #Other outputs				
    net_old <- adjGross - student_loan_old - income_tax_old - ni_old
    net_new <- new_adj_gross - student_loan_new - income_tax_new - ni_new
    
    basic_uplift <- (grade_mult -1) *100
    gross_uplift <- (new_gross/gross_pay -1) *100
    net_uplift <- (net_new/net_old -1) *100
    
    pension_accrued_old <- pension_base / 54
    pension_accrued_new <- new_pension_base / 54
    
    # Rendered Output
    output$salary_output <- renderPrint({

      cat("=== OLD Pay ===\n")
      cat("Gross Pay: £", formatC(gross_pay, format = "f", digits = 2),
          " (of which £", formatC(pensionable, format = "f", digits = 2), " is pensionable)\n\n", sep = "")
      cat("Gross pay is made of:\n")  						

	 if (!input$know_gross_income) {  # Show detailed OOH breakdown only if gross income wasn't entered
		cat("   Regular pay for", total_time ," hours: £", formatC(non_enhanced_pay, format = "f", digits = 2), "\n")
		if (enhanced_pay != 0) cat("   Of which,", enhanced_time," hours attract a 37% enhancement: £", formatC(enhanced_pay, format = "f", digits = 2), "\n")
		if (adj_weekend_pay != 0) cat("   Weekend Allowance: £", formatC(adj_weekend_pay, format = "f", digits = 2), "\n")
		if (adj_on_call != 0) cat("   On-call Availability: £", formatC(adj_on_call, format = "f", digits = 2), "\n")
	} else {
		cat("   Basic pay: £", formatC(adj_grade, format = "f", digits = 2), "\n")
		if (OOH_pay != 0) cat("   OOH Pay: £", formatC(OOH_pay, format = "f", digits = 2), "\n")
	}

      if (adj_premia != 0) cat("   Pay Premia: £", formatC(adj_premia, format = "f", digits = 2), "\n")
      if (ltft_premia != 0) cat("   LTFT Allowance: £", formatC(ltft_premia, format = "f", digits = 2), "\n")
      if (adj_weight != 0) cat("   London Weighting: £", formatC(adj_weight, format = "f", digits = 2), "\n")
      cat("\nThe following are the deductions made to your gross pay:\n")																 
      if (pension_ded != 0) cat("   Pension Deduction: £", formatC(pension_ded, format = "f", digits = 2), "\n")
      if (student_loan_old != 0) cat("   Student Loan: £", formatC(student_loan_old, format = "f", digits = 2), "\n")
      cat("   Income Tax: £", formatC(income_tax_old, format = "f", digits = 2), "\n")
      cat("   NI: £", formatC(ni_old, format = "f", digits = 2), "\n\n")
      
      #cat("\n","test data, comment out - taxable pay", formatC((gross_pay - pension_ded), format = "f", digits = 2), "\n\n")
      
      
      
      cat("Net Pay: £", formatC(net_old, format = "f", digits = 2), "\n")
      if (pension_accrued_old != 0) cat("Pension Accrued: £", formatC(pension_accrued_old, format = "f", digits = 2), "\n")
      
      cat("\n=== NEW Pay (Uplifted) ===\n")
      cat("Gross Pay: £", formatC(new_gross, format = "f", digits = 2),
          " (of which £", formatC(new_pensionable, format = "f", digits = 2), " is pensionable)\n\n", sep = "")
      cat("Gross pay is made of:\n")
      
	 if (!input$know_gross_income) {  # Show detailed OOH breakdown only if gross income wasn't entered
		cat("   Regular pay for", total_time ," hours: £", formatC(new_non_enhanced_pay, format = "f", digits = 2), "\n")
		if (new_enhanced_pay != 0) cat("   Of which,", enhanced_time," hours attract a 37% enhancement: £", formatC(new_enhanced_pay, format = "f", digits = 2), "\n")
		if (new_adj_weekend_pay != 0) cat("   Weekend Allowance: £", formatC(new_adj_weekend_pay, format = "f", digits = 2), "\n")
		if (new_adj_on_call != 0) cat("   On-call Availability: £", formatC(new_adj_on_call, format = "f", digits = 2), "\n")
	} else {
		cat("   Basic pay: £", formatC(new_adj_grade, format = "f", digits = 2), "\n")
		if (OOH_pay != 0) cat("   OOH Pay: £", formatC(new_OOH, format = "f", digits = 2), "\n")
	}
	 
      if (new_premia != 0) cat("   Pay Premia (Uplifted): £", formatC(new_premia, format = "f", digits = 2), "\n")
      if (ltft_premia != 0) cat("   LTFT Allowance: £", formatC(ltft_premia, format = "f", digits = 2), "\n")
      if (adj_weight != 0) cat("   London Weighting: £", formatC(adj_weight, format = "f", digits = 2), "\n")
      cat("\nThe following are the deductions made to your gross pay:\n")																	 
      if (new_pension_ded != 0) cat("   Pension Deduction: £", formatC(new_pension_ded, format = "f", digits = 2), "\n")
      if (student_loan_new != 0) cat("   Student Loan: £", formatC(student_loan_new, format = "f", digits = 2), "\n")
      cat("   Income Tax: £", formatC(income_tax_new, format = "f", digits = 2), "\n")
      cat("   NI: £", formatC(ni_new, format = "f", digits = 2), "\n\n")
      
      #cat("\n","test data, comment out - taxable pay", formatC((new_gross - new_pension_ded), format = "f", digits = 2), "\n\n")
	  #cat("\n","test data, comment out - basic hours  ", basic_hours, "hrs, and ltftmult*40", ltft_multiplier*40,"\n\n")
      #cat("\n","test data, comment out - gross less ooh  ", Gross_less_OOH,"\n\")
	  #cat("\n","test data, comment out - gross less ooh + OOH  ", Gross_less_OOH + OOH_pay,"\n\")
	  
      cat("Net Pay: £", formatC(net_new, format = "f", digits = 2), "\n")
      if (pension_accrued_new != 0) cat("Pension Accrued: £", formatC(pension_accrued_new, format = "f", digits = 2), "\n")
      
      cat("\n=== Overall Uplift ===\n")
      cat("Uplift to basic pay: ", round(basic_uplift,2), "%\n")																			  
      cat("Uplift to gross pay: ", round(gross_uplift,2), "%\n")
      cat("Uplift to net pay: ", round(net_uplift,2), "%\n")
	  cat("Net Pay Difference: £", round(net_new - net_old), "\n\n")
    })  # <-- This closes renderPrint
    
	
	#if pensionable pay is greater than gross pay
	output$pension_warning <- renderUI({
		if (pensionable > gross_pay || new_pensionable > new_gross) {
			HTML(
			'<div class="alert alert-warning" role="alert">
				⚠️ <strong>Note:</strong> Your pensionable pay is higher than your gross pay.
				Please read this <a href="https://www.westmidlandsdeanery.nhs.uk/support/less-than-full-time-training/less-than-full-time-training-guide/pensions" 
				target="_blank" class="alert-link">LTFT Pension info</a>.
			</div>'
			)
		} else {
			NULL
		}
	}) # closes pension warning
	
	#barplot render
		  output$barplot_output <- renderPlot({
    net_values <- c(net_old, net_new)
    deduction_values <- c(
      pension_ded + student_loan_old + income_tax_old + ni_old,
      new_pension_ded + student_loan_new + income_tax_new + ni_new
    )

    values_matrix <- rbind(Net = net_values, Deductions = deduction_values)

    
	par(mgp = c(4, 0.5, 0))  # Moves the y-axis label further from tick labels
	bp <- barplot(values_matrix,
            col = c("steelblue", "tomato"),
            names.arg = c("Pre-uplift", "Post-uplift"),
            ylim = c(0, max(colSums(values_matrix)) * 1.1),
            main = "Net Pay vs Deductions (Old vs New)",
            ylab = "(£)",
            legend.text = TRUE,
            args.legend = list(x = "topright"),
			yaxt = "n")
			
	# Add custom y-axis with comma formatting
			y_ticks <- pretty(c(0, max(colSums(values_matrix)) * 1.1))
			axis(2, at = y_ticks, labels = format(y_ticks, big.mark = ","), las = 1)
  
		# Add text labels on top of each bar segment
			cumul_values <- apply(values_matrix, 2, cumsum)
			for (i in 1:ncol(values_matrix)) {
				for (j in 1:nrow(values_matrix)) {
					height <- cumul_values[j, i]
					label <- format(values_matrix[j, i], big.mark = ",")
					text(x = bp[i], y = height - values_matrix[j, i]/2, labels = label, col = "white", cex = 0.9)
				}
			} # closes text value on bar segments
		}) # closes barplot render

	
  })  # <-- This closes observeEvent
  
}  # <-- This closes server

shinyApp(ui = ui, server = server)