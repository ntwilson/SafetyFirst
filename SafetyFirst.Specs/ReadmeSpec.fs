namespace SafetyFirst.Specs

/// <summary>
/// this just makes sure that everything in the README compiles
/// </summary>
module ReadmeSpec =
  open System
  open System.Data
  open SafetyFirst

  let divide (numerator:float) (denominator:float) =
    if denominator = 0.
    then Error "Cannot divide by 0!"
    else Ok (numerator / denominator)

  type Invoice = { Total:float; NumberOfUnits:float }

  let pricePerUnitForDisplay invoice =
    match divide invoice.Total invoice.NumberOfUnits with
    | Ok ppu -> ppu.ToString()
    | Error err -> "N/A: " + err

  let pricePerUnit invoice = divide invoice.Total invoice.NumberOfUnits
  let savingsPerUnit invoice dollarsOff =
    pricePerUnit invoice |> Result.bind (fun ppu -> divide dollarsOff ppu)
    // you could of course `pricePerUnit () |> Result.bind (divide dollarsOff)`
    // but I find it counterintuitive that dollarsOff would be the numerator with that syntax

  let pricePerUnitWithDiscount invoice dollarsOffPerUnit =
    pricePerUnit invoice |> Result.map (fun ppu -> ppu - dollarsOffPerUnit)

  let newInvoice total numberOfUnits = { Total = total; NumberOfUnits = numberOfUnits }

  let createInvoice2 (total:Result<double, string>) (numberOfUnits:Result<double, string>) =
    Result.map2 newInvoice total numberOfUnits

  let executeDatabaseQuery sql = 
    Result<DataTable, string>.Error "no query"
    

  type Logger() = member this.Log err = ()
  let logger = new Logger ()
  let sql = ""

  let result:Result<DataTable, string> = executeDatabaseQuery sql
  result |> Result.ifError (logger.Log);

  let savingsPerUnit2 invoice dollarsOff =
    Result.expr {
      let! ppu = pricePerUnit invoice
      return! divide dollarsOff ppu
    }

  let pricePerUnitWithDiscount2 invoice dollarsOffPerUnit =
    Result.expr {
      let! ppu = pricePerUnit invoice
      return ppu - dollarsOffPerUnit
    }

  let createInvoice3 (total:Result<double, string>) (numberOfUnits:Result<double, string>) =
    Result.expr {
      let! t = total
      let! n = numberOfUnits
      return newInvoice t n
    }

