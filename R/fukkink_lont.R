#' Does training matter? A meta-analysis of caregiver training studies
#'
#' A review of 17 experimental studies published between 1980 and 2005 on the
#' effect of specialized training on the competency of caregivers in childcare.
#'
#' \tabular{lll}{
#'    \strong{id_exp} \tab \code{integer} \tab Unique identifier of the study\cr
#'    \strong{yi} \tab \code{numeric} \tab Standardized mean difference between the control group and\cr
#'    \strong{vi} \tab \code{numeric} \tab Variance of the effect size\cr
#'    \strong{Journal} \tab \code{factor} \tab Publication type (scientific journal or other publications)\cr
#'    \strong{Setting} \tab \code{factor} \tab Setting (center-based care or family daycare)\cr
#'    \strong{Integrated} \tab \code{factor} \tab Whether the training was integrated into childcare practice\cr
#'    \strong{Supervision} \tab \code{factor} \tab Whether supervision was part of the training\cr
#'    \strong{Scope} \tab \code{factor} \tab Scope of the training (narrow or broad)\cr
#'    \strong{Location} \tab \code{factor} \tab Location of the training (one-site or multi-site)\cr
#'    \strong{Curriculum} \tab \code{factor} \tab Fixed curriculum\cr
#'    \strong{Control} \tab \code{factor} \tab Alternative treatment for control group\cr
#'    \strong{Assignment} \tab \code{factor} \tab Random assignment or matching (at the level of the individual caregiver or childcare center)\cr
#'    \strong{Train_Knowledge} \tab \code{factor} \tab Explicit focus on knowledge\cr
#'    \strong{Train_Skills} \tab \code{factor} \tab Explicit focus on skills\cr
#'    \strong{Train_Attitude} \tab \code{factor} \tab Explicit focus on attitude\cr
#'    \strong{Video} \tab \code{factor} \tab Use of video feedback\cr
#'    \strong{Design} \tab \code{factor} \tab Single group, or two-group experimental design\cr
#'    \strong{Pre_Post} \tab \code{factor} \tab Pretest/posttest design (yes/no)\cr
#'    \strong{Blind} \tab \code{factor} \tab Was a blinding procedure used?\cr
#'    \strong{Attrition} \tab \code{numeric} \tab Attrition from the experimental condition (percentage)\cr
#'    \strong{Pretest_es} \tab \code{numeric} \tab Pre-test effect size\cr
#'    \strong{Self_report} \tab \code{factor} \tab Self-report measures of caregiver competencies versus ‘objective’ test or observation by independent observer\cr
#'    \strong{DV_Knowledge} \tab \code{factor} \tab Test focused on knowledge\cr
#'    \strong{DV_Skills} \tab \code{factor} \tab Test focused skills\cr
#'    \strong{DV_Attitude} \tab \code{factor} \tab Test focused on attitudes\cr
#'    \strong{DV_Aligned} \tab \code{factor} \tab Test aligned with the content of the training (yes/no)\cr
#'    \strong{Two_group_design} \tab \code{factor} \tab Single group, or two-group experimental design\cr
#'    \strong{Trainee_Age} \tab \code{numeric} \tab Trainees’ age \cr
#'    \strong{Trainee_Experience} \tab \code{numeric} \tab Trainees’ working experience\cr
#'    \strong{n_total} \tab \code{integer} \tab Total n at post-test
#' }
#' @docType data
#' @keywords datasets
#' @name fukkink_lont
#' @usage data(fukkink_lont)
#' @references Fukkink, R. G., & Lont, A. (2007). Does training matter? A
#' meta-analysis and review of caregiver training studies.
#' Early childhood research quarterly, 22(3), 294-311.
#' \href{https://doi.org/10.1016/j.ecresq.2007.04.005}{
#' doi:10.1016/j.ecresq.2007.04.005}
#' @source \href{https://doi.org/10.1016/j.ecresq.2007.04.005}{
#' doi:10.1016/j.ecresq.2007.04.005}
#' @format A data.frame with 78 rows and 30 columns.
NULL
