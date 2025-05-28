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
  type = c("none", "GP", "Psych Core", "Psych HST (3 years)", "Psych HST (4 years)", "Academia", "Histopathology", 
           "EM/OMFS (3 years)", "EM/OMFS (4 years)", "EM/OMFS (5 years)", 
           "EM/OMFS (6 years)", "EM/OMFS (7 years)", "EM/OMFS (8 years)"),
  f_premia = c(0, 10690, 4347, 4347, 3260, 5216, 5216, 8693, 6520, 5216, 4347, 3726, 3260)
)

L_weight <- 2162
Parttime_alwn <- 1000

# Constants
PERSONAL_ALLOWANCE <- 12570
TAPER_START <- 100000
TAPER_END <- 125140
BASIC_RATE <- 0.2
HIGHER_RATE <- 0.4
ADDITIONAL_RATE <- 0.45

INCOME_THRESHOLD <- 12570
INCOME_BASIC_THRESHOLD <- 50270
BASIC_BAND <- INCOME_BASIC_THRESHOLD - INCOME_THRESHOLD
HIGHER_BAND <- TAPER_END - INCOME_BASIC_THRESHOLD

NI_THRESHOLD <- 12570
NI_BASIC_LIMIT <- 50270
NI_RATE1 <- 0.08
NI_RATE2 <- 0.02

P1_THRESH <-26065
P2_THRESH <-28470
P4_THRESH <-32745
P5_THRESH <-25000
PG_THRESH <-21000

# UI	
ui <- bslib::page_fluid(
  titlePanel("Resident Doctor Pay Uplift Calculator - England 25/26"),
  sidebarLayout(
    sidebarPanel(
	  width = 4,
      h6(strong("Select Grade")),
	  radioButtons("grade", label = NULL, choices = pay_scales$grade),
      numericInput("gross_payA", "Total Pay (£)", value = 36616, min = 0, max = Inf),

      uiOutput("premia_selector"),
      
      h6(strong("London Weighting")),
	  checkboxInput("is_London_weight", "Do you get London Weighting?", value = FALSE),
	  h6(strong("Pension Options")),
      checkboxInput("is_under_StatePensionAge", "Are you under state pension age?", value = TRUE),
      conditionalPanel(
        condition = "input.is_under_StatePensionAge == true",
        checkboxInput("is_opt_in_pension", "Are you opted in to the pension?", value = TRUE)
      ),
	  
	  h6(strong("LTFT Options")),
      checkboxInput("is_ltft", "Are you less than full time?", value = FALSE),
      conditionalPanel(
        condition = "input.is_ltft == true",
        numericInput("ltft_percentageA", "LTFT Percentage (%) (100 if full time)", value = 100, min = 0, max = 100),
        numericInput("ltft_hoursA", "LTFT Total Hours (use 40 if Full time)", value = 40, min = 0)
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
      verbatimTextOutput("salary_output")
    )
  )
)

server <- function(input, output, session) {

#Flexible premia selector
output$premia_selector <- renderUI({
  grade <- input$grade
  common_choices <- c("none", "Academia")
  if (grade %in% c("FY1", "FY2")) {
    choices <- common_choices
  } else {
    extra_choices <- setdiff(pay_premia$type, common_choices)
    choices <- c(common_choices, extra_choices)
  }

  tagList(
    div(
      style = "display: flex; align-items: center; gap: 8px;",
      h6(strong("Flexible Pay Premia Type")),
      tags$span(
        bs_icon("info-circle", size = "1.25em", class = "text-primary"),
        title = "You must deselect 'none' to choose a premium.\nIf you reselect 'none', it will deselect all other options.\nYou can select two options, but only if one of them is Academia.",
        `data-bs-toggle` = "tooltip",
        `data-bs-placement` = "right",
        style = "cursor: pointer;"
      )
    ),
    checkboxGroupInput(
      "flexible_pay_premia",
      label = NULL,
      choices = choices,
      selected = if (grade %in% c("FY1", "FY2")) "none" else NULL
    )
  )
})

  observe({
    selected <- input$flexible_pay_premia
    grade <- input$grade
    if (is.null(selected)) return()
    # If "none" is selected with others, keep only "none"														 
    if ("none" %in% selected && length(selected) > 1) {
      updateCheckboxGroupInput(session, "flexible_pay_premia", selected = "none")
      return()
    }
	# For non-FY1/FY2 grades, enforce selection rule:												 
    if (!(grade %in% c("FY1", "FY2"))) {
	  # Allow max 2, one of which must be "Academia" if both are selected																	 
      if (length(selected) > 2) {
        updateCheckboxGroupInput(session, "flexible_pay_premia", selected = selected[1:2])
      } else if (!("Academia" %in% selected) && length(selected) > 1) {
        updateCheckboxGroupInput(session, "flexible_pay_premia", selected = selected[1])
      }
    }
  })

  observeEvent(input$calculate, {
	#inputs			 
    base_salary <- pay_scales %>% filter(grade == input$grade) %>% pull(base_salary)
    selected_premia <- sum(pay_premia %>% filter(type %in% input$flexible_pay_premia) %>% pull(f_premia))
    selected_weight <- if (input$is_London_weight) 2162 else 0
    selected_ltft <- if (input$is_ltft) 1000 else 0
    ltft_percentage <- if (input$is_ltft) input$ltft_percentageA else 100
    ltft_hours <- if (input$is_ltft) input$ltft_hoursA else 40

    ltft_multiplier <- ltft_percentage / 100
    pension_factor <- min(ltft_hours / 40, 1)
	#adjustments			 
    adj_grade <- base_salary * ltft_multiplier
    adj_premia <- selected_premia * ltft_multiplier
    adj_weight <- selected_weight * ltft_multiplier
	
	#Gross correction if human error
	Gross_less_OOH <- adj_grade + adj_premia + adj_weight + selected_ltft
    gross_pay <- input$gross_payA
	was_corrected <- FALSE
	if (gross_pay < Gross_less_OOH) {
		gross_pay <- Gross_less_OOH
		was_corrected <- TRUE
	}

	#Pension opt in/out		   
    pension_opt <- ifelse(!input$is_under_StatePensionAge || !input$is_opt_in_pension, 0, 1)																	  
    pensionable <- (base_salary * pension_factor) + adj_weight
    pension_base <- ifelse(pension_opt < 1, 0, pensionable)
	
	#More Pay calcs			
    OOH_pay <- gross_pay - Gross_less_OOH											 

	#Uplift calcs				  
    uplifted_grade <- ceiling(base_salary * 1.04 + 750)
    new_adj_grade <- uplifted_grade * ltft_multiplier
    new_premia <- ceiling(selected_premia * 1.04) * ltft_multiplier
    grade_mult <- uplifted_grade / base_salary
    new_OOH <- OOH_pay * grade_mult
    new_gross <- new_adj_grade + new_OOH + new_premia + selected_ltft + adj_weight
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

	# Income Tax and NI						   
	income_tax <- function(gross) {
		pa <- PERSONAL_ALLOWANCE
		if (gross > TAPER_START && gross <= TAPER_END) {
			pa <- max(0, PERSONAL_ALLOWANCE - (gross - TAPER_START) / 2)
		} else if (gross > TAPER_END) {
			pa <- 0
		}

		taxable_income <- max(0, gross - pa)

		basic_band <- BASIC_BAND
		higher_band <- HIGHER_BAND

		if (taxable_income <= basic_band) {
			return(taxable_income * BASIC_RATE)
		} else if (taxable_income <= (basic_band + higher_band)) {
			return(basic_band * BASIC_RATE + (taxable_income - basic_band) * HIGHER_RATE)
		} else {
		return(
			basic_band * BASIC_RATE +
			higher_band * HIGHER_RATE +
			(taxable_income - basic_band - higher_band) * ADDITIONAL_RATE
			)
		}
	}

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

    income_tax_old <- income_tax(adjGross)
    income_tax_new <- income_tax(new_adj_gross)
    ni_old <- ifelse(input$is_under_StatePensionAge, ni_calc(adjGross), 0)
    ni_new <- ifelse(input$is_under_StatePensionAge, ni_calc(new_adj_gross), 0)

	#Other outputs				
	net_old <- adjGross - student_loan_old - income_tax_old - ni_old
	net_new <- new_adj_gross - student_loan_new - income_tax_new - ni_new
	
	basic_uplift <- (new_adj_grade/adj_grade -1) *100
	gross_uplift <- (new_adj_gross/adjGross -1) *100
	net_uplift <- (net_new/net_old -1) *100

    pension_accrued_old <- pensionable / 54
    pension_accrued_new <- new_pensionable / 54

	# Rendered Output
	output$salary_output <- renderPrint({
	if (was_corrected) {
		cat("⚠️  Note: The total pay you entered was below the minimum expected based on base pay, premia, weighting, and LTFT allowance.\n")
		cat("   It was automatically increased to match those components.\n\n")
	}

	cat("=== OLD Pay ===\n")
			cat("Gross Pay: £", formatC(gross_pay, format = "f", digits = 2),
		" (of which £", formatC(pension_base, format = "f", digits = 2), " is pensionable)\n\n", sep = "")
		cat("Gross pay is made of:\n")
			cat("   Basic pay: £", formatC(adj_grade, format = "f", digits = 2), "\n")  						
			if (OOH_pay != 0) cat("   OOH Pay: £", formatC(OOH_pay, format = "f", digits = 2), "\n")
			if (adj_premia != 0) cat("   Pay Premia: £", formatC(adj_premia, format = "f", digits = 2), "\n")
			if (selected_ltft != 0) cat("   LTFT Allowance: £", formatC(selected_ltft, format = "f", digits = 2), "\n")
			if (adj_weight != 0) cat("   London Weighting: £", formatC(adj_weight, format = "f", digits = 2), "\n")
		cat("\nThe following are the deductions made to your gross pay:\n")																 
			if (pension_ded != 0) cat("   Pension Deduction: £", formatC(pension_ded, format = "f", digits = 2), "\n")
			if (student_loan_old != 0) cat("   Student Loan: £", formatC(student_loan_old, format = "f", digits = 2), "\n")
			cat("   Income Tax: £", formatC(income_tax_old, format = "f", digits = 2), "\n")
			cat("   NI: £", formatC(ni_old, format = "f", digits = 2), "\n\n")
			
			cat("Net Pay: £", formatC(net_old, format = "f", digits = 2), "\n")
			if (pension_accrued_old != 0) cat("Pension Accrued: £", formatC(pension_accrued_old, format = "f", digits = 2), "\n")

	cat("\n=== NEW Pay (Uplifted) ===\n")
			cat("Gross Pay: £", formatC(new_gross, format = "f", digits = 2),
		" (of which £", formatC(new_pension_base, format = "f", digits = 2), " is pensionable)\n\n", sep = "")
		cat("Gross pay is made of:\n")
			cat("   Basic pay: £", formatC(new_adj_grade, format = "f", digits = 2), "\n")																				   
			if (new_OOH != 0) cat("   OOH Pay (Uplifted): £", formatC(new_OOH, format = "f", digits = 2), "\n")
			if (new_premia != 0) cat("   Pay Premia (Uplifted): £", formatC(new_premia, format = "f", digits = 2), "\n")
			if (selected_ltft != 0) cat("   LTFT Allowance: £", formatC(selected_ltft, format = "f", digits = 2), "\n")
			if (adj_weight != 0) cat("   London Weighting: £", formatC(adj_weight, format = "f", digits = 2), "\n")
		cat("\nThe following are the deductions made to your gross pay:\n")																	 
			if (new_pension_ded != 0) cat("   Pension Deduction: £", formatC(new_pension_ded, format = "f", digits = 2), "\n")
			if (student_loan_new != 0) cat("   Student Loan: £", formatC(student_loan_new, format = "f", digits = 2), "\n")
			cat("   Income Tax: £", formatC(income_tax_new, format = "f", digits = 2), "\n")
			cat("   NI: £", formatC(ni_new, format = "f", digits = 2), "\n\n")
			
			cat("Net Pay: £", formatC(net_new, format = "f", digits = 2), "\n")
			if (pension_accrued_new != 0) cat("Pension Accrued: £", formatC(pension_accrued_new, format = "f", digits = 2), "\n")

	cat("\n=== Overall Uplift ===\n")
		cat("Uplift to basic pay: ", round(basic_uplift,2), "%\n")																			  
		cat("Uplift to gross pay (%): ", round(gross_uplift,2), "\n")
		cat("Uplift to net pay (%): ", round(net_uplift,2), "\n")
})  # <-- This closes renderPrint

})  # <-- This closes observeEvent

}  # <-- This closes server

shinyApp(ui = ui, server = server)
