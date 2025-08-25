namespace ReservationService.Tests

open System
open FsCheck.Xunit

// A configurable Property attribute that reads test intensity from environment variables at runtime
// Use env vars to control without code changes:
//   FS_MAX_TESTS   -> overrides MaxTest (default: keep existing or FsCheck default)
//   FS_START_SIZE  -> overrides StartSize
//   FS_END_SIZE    -> overrides EndSize
//   FS_QUIET       -> if set to "1", enables QuietOnSuccess
// Example:
//   FS_MAX_TESTS=100000 FS_END_SIZE=10000 dotnet test -v minimal --filter FullyQualifiedName~YourSuite

[<System.AttributeUsage(System.AttributeTargets.Method, AllowMultiple=false)>]
type ConfigurablePropertyAttribute() as this =
    inherit PropertyAttribute()

    let tryGetInt (name:string) (fallback:int) =
        match Environment.GetEnvironmentVariable(name) with
        | null | "" -> fallback
        | s -> match System.Int32.TryParse(s) with | true, v -> v | _ -> fallback

    do
        // Read current defaults from base attribute
        let baseMax   = this.MaxTest
        let baseStart = this.StartSize
        let baseEnd   = this.EndSize

        // Override from environment if present
        let envMax   = tryGetInt "FS_MAX_TESTS" baseMax
        let envStart = tryGetInt "FS_START_SIZE" baseStart
        let envEnd   = tryGetInt "FS_END_SIZE" baseEnd

        this.MaxTest <- envMax
        this.StartSize <- envStart
        this.EndSize <- envEnd

        match Environment.GetEnvironmentVariable("FS_QUIET") with
        | null | "" -> ()
        | v when v = "1" || v.Equals("true", StringComparison.OrdinalIgnoreCase) -> this.QuietOnSuccess <- true
        | _ -> ()

