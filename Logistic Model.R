
```{r}
library(lme4)
library(broom.mixed)
library(MuMIn)
library(dplyr)

data<-read.csv("/Users/rcave/Downloads/Metadata.csv")

cand.vars <- c(
  "Age",
  "Population.density..km..",
  "Vaccinetype",
  "Year",
  "Number.of.children.age..5.or.under.in.a.household",
  "Penicillin.MIC..??g.mL.",
  "Social.economic.score",
  "Erythromycin.and.Tetracycline",
  "Crowd.index",
  "Number.of.adults.in.a.household",
  "Number.of.children.age..5.to.15..in.a.household"
)



# --- 0. Identify continuous variables and scale them once ---
cont_vars <- cand.vars[sapply(data[cand.vars], is.numeric)]

# Scale continuous vars in data, add suffix "_z"
for (v in cont_vars) {
  data[[paste0(v, "_z")]] <- scale(data[[v]])
}

# --- 1. Fit univariable GLMMs with scaled variables for continuous predictors ---
unis <- lapply(cand.vars, function(var) {
  
  # Use scaled var if numeric
  if (var %in% cont_vars) {
    var_use <- paste0(var, "_z")
  } else {
    var_use <- var
  }
  
  fmla <- as.formula(paste0("Less_than_saturation_point ~ ", var_use, " + (1|GPSC)"))
  
  m <- glmer(fmla, data = data, family = binomial,
             control = glmerControl(optimizer = "bobyqa"))
  
  res <- broom.mixed::tidy(m, effects = "fixed", conf.int = TRUE, exponentiate = TRUE)
  
  res <- res %>%
    filter(term != "(Intercept)") %>%
    mutate(predictor = var) %>%
    select(term, OR = estimate, SE = std.error, z = statistic, 
           `p-value` = p.value, `2.5 %` = conf.low, `97.5 %` = conf.high, predictor)
  
  return(res)
})

uni_table <- bind_rows(unis)

# --- 2. Filter variables with p < 0.1 ---
top.vars <- uni_table %>%
  filter(`p-value` < 0.1) %>%
  pull(predictor) %>%
  unique()

# For multivariable model, use scaled vars if numeric
top.vars_scaled <- sapply(top.vars, function(v) {
  if (v %in% cont_vars) paste0(v, "_z") else v
})

# --- 3. Build global multivariable GLMM ---
global_fmla <- as.formula(
  paste0("Less_than_saturation_point ~ ",
         paste(top.vars_scaled, collapse = " + "),
         " + (1|GPSC)")
)

global_mod <- glmer(global_fmla, data = data, family = binomial,
                    control = glmerControl(optimizer = "bobyqa"),
                    na.action = na.fail)

# --- 4. Subset model selection with dredge ---
dredged <- dredge(global_mod, rank = "AICc", trace = FALSE)

print(head(dredged, 5))

best_mod <- get.models(dredged, 1)[[1]]

# --- 5. Summarise best multivariable model ---
best_sum <- tidy(best_mod, effects = "fixed", conf.int = TRUE, exponentiate = TRUE) %>%
  filter(term != "(Intercept)") %>%
  select(term, estimate, conf.low, conf.high, p.value) %>%
  rename(
    OR = estimate,
    `2.5 %` = conf.low,
    `97.5 %` = conf.high,
    `p-value` = p.value
  )

print(best_sum)
print(uni_table)
```

