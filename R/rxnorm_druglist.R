#' List of Medications
#'
#' A dictionary that contains a vector of medication names, primarily derived from RxNorm.
#'
#'
#' RxNorm is provided by the U.S. National Library of Medicine. This dictionary
#' uses the January 7, 2019 RxNorm files directly downloaded from
#' \href{https://www.nlm.nih.gov/research/umls/rxnorm/docs/rxnormfiles.html}{https://www.nlm.nih.gov/research/umls/rxnorm/docs/rxnormfiles.html}.
#'
#' This list contains ingredient and brand names, cleaned to remove expressions likely
#' to be ambiguous (e.g., 'today' or 'date'). It has also been supplemented with abbreviations
#' for various medications in a manually curated list from Vanderbilt University's Synthetic Derivative.
#'
#' This product uses publicly available data courtesy of the U.S. National Library of Medicine (NLM), National
#' Institutes of Health, Department of Health and Human Services; NLM is not responsible for the product and
#' does not endorse or recommend this or any other product.
#'
#' @format A vector with 59,326 character strings.
#'
#' @keywords datasets
#'
#' @references
#' Nelson SJ, Zeng K, Kilbourne J, Powell T, Moore R. Normalized names for clinical drugs: RxNorm at 6 years.
#' J Am Med Inform Assoc. 2011 Jul-Aug;18(4)441-8. doi: 10.1136/amiajnl-2011-000116. Epub 2011 Apr 21.
#' PubMed PMID: 21515544; PubMed Central PMCID: PMC3128404.
#'
#' @examples
#' data(rxnorm_druglist)
"rxnorm_druglist"