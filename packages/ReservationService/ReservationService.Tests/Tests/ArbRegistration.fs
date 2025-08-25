namespace ReservationService.Tests

open FsCheck.Xunit

// Register our custom Arbitraries and set fast defaults for the whole test assembly
[<assembly: Properties(
    Arbitrary = [| typeof<DomainGenerators.DomainArbitraries> |],
    MaxTest = 20,
    StartSize = 1,
    EndSize = 20,
    QuietOnSuccess = true
)>]
do()
