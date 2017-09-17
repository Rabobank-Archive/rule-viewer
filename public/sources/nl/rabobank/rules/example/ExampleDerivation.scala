package nl.rabobank.rules.example

import nl.rabobank.rules.dsl.nl.grammar._
import nl.rabobank.rules.finance.nl._

import nl.rabobank.rules.example.ExampleGlossary._
import nl.rabobank.rules.dsl.ext.ListDerivationHelper._

class ExampleDerivation extends Berekening (

  Gegeven (altijd)
  Bereken
    DefaultPaidHealthCost is 0.euro en
    DefaultMinimumOwedTaxes is 0.euro
  ,
  Gegeven (altijd)
  Bereken
    BaseIncomeTax is BaseIncome * FlatTaxRate en
    BaseHealthCosts is eerste(TotalPaidHealthCost, DefaultPaidHealthCost) en
    HealthCostEligibleForReimbursement is BaseHealthCosts * HealthCostReimbursementPercentage en
    ActualHealthCostReimbursement is laagste.van(List(HealthCostEligibleForReimbursement, HealthCostReimbursementCeiling)) en
    TaxesReducedByReimbursements is BaseIncomeTax - ActualHealthCostReimbursement en
    LegallyOwedTaxes is (hoogste van(List(TaxesReducedByReimbursements, DefaultMinimumOwedTaxes)))
  ,
  Gegeven (LegallyOwedTaxes < TotalPrepaidTaxes)
  Bereken
    TaxReturnAmount is TotalPrepaidTaxes - LegallyOwedTaxes
  ,
  Gegeven (LegallyOwedTaxes >= TotalPrepaidTaxes)
  Bereken
    TaxDueAmount is LegallyOwedTaxes - TotalPrepaidTaxes


)
