namespace ReservationService.Tests

open FsCheck.Xunit

// Register our custom Arbitraries for the whole test assembly
[<assembly: Properties(Arbitrary = [| typeof<ReservationService.Tests.Tests.DomainGenerators.DomainArbitraries> |])>]
do()
