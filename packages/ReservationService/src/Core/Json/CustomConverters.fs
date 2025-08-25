namespace ReservationService.Core.Json

open System
open System.Text.Json
open System.Text.Json.Serialization
open ReservationService.Core.Types

module JsonConverters =

    // ---------- NonEmptyString Converter ----------
    type NonEmptyStringConverter() =
        inherit JsonConverter<NonEmptyString>()
        override _.Read(reader: byref<Utf8JsonReader>, _typeToConvert, _options) : NonEmptyString =
            if reader.TokenType <> JsonTokenType.String then
                raise (JsonException("Expected string for NonEmptyString"))
            let s = reader.GetString()
            match NonEmptyString.Create(s) with
            | Result.Ok v -> v
            | Result.Error msg -> raise (JsonException($"Invalid NonEmptyString: {msg}"))
        override _.Write(writer: Utf8JsonWriter, value: NonEmptyString, _options) : unit =
            writer.WriteStringValue(value.Value)

    // ---------- DU Converters (as simple strings) ----------
    
    type ValidationPhaseConverter() =
        inherit JsonConverter<ValidationIo.ValidationPhase>()
        override _.Read(reader: byref<Utf8JsonReader>, _t, _o) =
            if reader.TokenType <> JsonTokenType.String then raise (JsonException("Expected string for ValidationPhase"))
            match reader.GetString() with
            | "Validated" -> ValidationIo.ValidationPhase.Validated
            | "Invalid" -> ValidationIo.ValidationPhase.Invalid
            | x -> raise (JsonException($"Unknown ValidationPhase: {x}"))
        override _.Write(writer: Utf8JsonWriter, v: ValidationIo.ValidationPhase, _o) =
            match v with
            | ValidationIo.ValidationPhase.Validated -> writer.WriteStringValue("Validated")
            | ValidationIo.ValidationPhase.Invalid -> writer.WriteStringValue("Invalid")

    type PipelineStepConverter() =
        inherit JsonConverter<ValidationIo.PipelineStep>()
        override _.Read(reader: byref<Utf8JsonReader>, _t, _o) =
            if reader.TokenType <> JsonTokenType.String then raise (JsonException("Expected string for PipelineStep"))
            match reader.GetString() with
            | "Pricing" -> ValidationIo.PipelineStep.Pricing
            | "Availability" -> ValidationIo.PipelineStep.Availability
            | "Approval" -> ValidationIo.PipelineStep.Approval
            | "Enrichment" -> ValidationIo.PipelineStep.Enrichment
            | s when s.StartsWith("Custom:") -> ValidationIo.PipelineStep.Custom(s.Substring("Custom:".Length))
            | x -> raise (JsonException($"Unknown PipelineStep: {x}"))
        override _.Write(writer: Utf8JsonWriter, v: ValidationIo.PipelineStep, _o) =
            match v with
            | ValidationIo.PipelineStep.Pricing -> writer.WriteStringValue("Pricing")
            | ValidationIo.PipelineStep.Availability -> writer.WriteStringValue("Availability")
            | ValidationIo.PipelineStep.Approval -> writer.WriteStringValue("Approval")
            | ValidationIo.PipelineStep.Enrichment -> writer.WriteStringValue("Enrichment")
            | ValidationIo.PipelineStep.Custom s -> writer.WriteStringValue($"Custom:{s}")

    type SeverityDtoConverter() =
        inherit JsonConverter<ValidationIo.SeverityDto>()
        override _.Read(reader: byref<Utf8JsonReader>, _t, _o) =
            if reader.TokenType <> JsonTokenType.String then raise (JsonException("Expected string for Severity"))
            match reader.GetString() with
            | "Info" -> ValidationIo.SeverityDto.Info
            | "Warning" -> ValidationIo.SeverityDto.Warning
            | "Error" -> ValidationIo.SeverityDto.Error
            | "Critical" -> ValidationIo.SeverityDto.Critical
            | x -> raise (JsonException($"Unknown Severity: {x}"))
        override _.Write(writer: Utf8JsonWriter, v: ValidationIo.SeverityDto, _o) =
            match v with
            | ValidationIo.SeverityDto.Info -> writer.WriteStringValue("Info")
            | ValidationIo.SeverityDto.Warning -> writer.WriteStringValue("Warning")
            | ValidationIo.SeverityDto.Error -> writer.WriteStringValue("Error")
            | ValidationIo.SeverityDto.Critical -> writer.WriteStringValue("Critical")

    type ValidationStatusDtoConverter() =
        inherit JsonConverter<ValidationIo.ValidationStatusDto>()
        override _.Read(reader: byref<Utf8JsonReader>, _t, _o) =
            if reader.TokenType <> JsonTokenType.String then raise (JsonException("Expected string for ValidationStatus"))
            match reader.GetString() with
            | "Pending" -> ValidationIo.ValidationStatusDto.Pending
            | "InProgress" -> ValidationIo.ValidationStatusDto.InProgress
            | "Succeeded" -> ValidationIo.ValidationStatusDto.Succeeded
            | "Failed" -> ValidationIo.ValidationStatusDto.Failed
            | "Skipped" -> ValidationIo.ValidationStatusDto.Skipped
            | x -> raise (JsonException($"Unknown ValidationStatus: {x}"))
        override _.Write(writer: Utf8JsonWriter, v: ValidationIo.ValidationStatusDto, _o) =
            match v with
            | ValidationIo.ValidationStatusDto.Pending -> writer.WriteStringValue("Pending")
            | ValidationIo.ValidationStatusDto.InProgress -> writer.WriteStringValue("InProgress")
            | ValidationIo.ValidationStatusDto.Succeeded -> writer.WriteStringValue("Succeeded")
            | ValidationIo.ValidationStatusDto.Failed -> writer.WriteStringValue("Failed")
            | ValidationIo.ValidationStatusDto.Skipped -> writer.WriteStringValue("Skipped")

    // Registration helper
    let registerAll (options: JsonSerializerOptions) =
        options.Converters.Add(NonEmptyStringConverter())
        options.Converters.Add(ValidationPhaseConverter())
        options.Converters.Add(PipelineStepConverter())
        options.Converters.Add(SeverityDtoConverter())
        options.Converters.Add(ValidationStatusDtoConverter())
        ()

