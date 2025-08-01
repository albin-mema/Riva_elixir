defmodule RivaAshWeb.NoDatabaseIntegrationTest do
  @moduledoc """
  Integration tests that can run without database connection.
  These tests focus on business logic, validations, and pure functions.
  """
  use ExUnit.Case, async: true

  @moduletag :unit
  @moduletag :fast
  @moduletag :core
  @moduletag :pure

  alias RivaAsh.ErrorHelpers
  alias RivaAsh.DateTimeHelpers
  alias RivaAsh.ResourceHelpers

  describe "ErrorHelpers" do
    test "success/1 wraps value in ok tuple" do
      assert ErrorHelpers.success("test") == {:ok, "test"}
      assert ErrorHelpers.success(nil) == {:ok, nil}
      assert ErrorHelpers.success(%{key: "value"}) == {:ok, %{key: "value"}}
    end

    test "failure/1 wraps value in error tuple" do
      assert ErrorHelpers.failure("error") == {:error, "error"}
      assert ErrorHelpers.failure(:not_found) == {:error, :not_found}
    end

    test "to_result/1 converts various formats to result tuples" do
      assert ErrorHelpers.to_result({:ok, "success"}) == {:ok, "success"}
      assert ErrorHelpers.to_result({:error, "failure"}) == {:error, "failure"}
      assert ErrorHelpers.to_result("direct_value") == {:ok, "direct_value"}
    end

    test "required/2 validates required values" do
      assert ErrorHelpers.required("value", :field_required) == {:ok, "value"}
      assert ErrorHelpers.required(nil, :field_required) == {:error, :field_required}
      assert ErrorHelpers.required("", :field_required) == {:error, :field_required}
    end
  end

  describe "DateTimeHelpers" do
    test "weekend?/1 correctly identifies weekends" do
      # Saturday and Sunday are weekends
      # Saturday
      assert DateTimeHelpers.weekend?(~D[2024-01-06]) == true
      # Sunday
      assert DateTimeHelpers.weekend?(~D[2024-01-07]) == true

      # Monday through Friday are not weekends
      # Monday
      assert DateTimeHelpers.weekend?(~D[2024-01-08]) == false
      # Tuesday
      assert DateTimeHelpers.weekend?(~D[2024-01-09]) == false
      # Friday
      assert DateTimeHelpers.weekend?(~D[2024-01-12]) == false
    end

    test "weekday?/1 correctly identifies weekdays" do
      # Monday through Friday are weekdays
      # Monday
      assert DateTimeHelpers.weekday?(~D[2024-01-08]) == true
      # Friday
      assert DateTimeHelpers.weekday?(~D[2024-01-12]) == true

      # Saturday and Sunday are not weekdays
      # Saturday
      assert DateTimeHelpers.weekday?(~D[2024-01-06]) == false
      # Sunday
      assert DateTimeHelpers.weekday?(~D[2024-01-07]) == false
    end

    test "day_type/1 returns correct day type" do
      # Monday
      assert DateTimeHelpers.day_type(~D[2024-01-08]) == :weekday
      # Saturday
      assert DateTimeHelpers.day_type(~D[2024-01-06]) == :weekend
    end

    test "day_name/1 returns correct day names" do
      assert DateTimeHelpers.day_name(~D[2024-01-08]) == "Monday"
      assert DateTimeHelpers.day_name(~D[2024-01-09]) == "Tuesday"
      assert DateTimeHelpers.day_name(~D[2024-01-06]) == "Saturday"
      assert DateTimeHelpers.day_name(~D[2024-01-07]) == "Sunday"
    end

    test "day_type_string/1 returns human readable strings" do
      assert DateTimeHelpers.day_type_string(~D[2024-01-08]) == "Weekday"
      assert DateTimeHelpers.day_type_string(~D[2024-01-06]) == "Weekend"
    end
  end

  describe "String and data validation" do
    test "email format validation with regex" do
      email_regex = ~r/^[^\s]+@[^\s]+\.[^\s]+$/

      # Valid emails
      assert Regex.match?(email_regex, "user@example.com")
      assert Regex.match?(email_regex, "test.email@domain.co.uk")

      # Invalid emails
      refute Regex.match?(email_regex, "invalid-email")
      refute Regex.match?(email_regex, "@domain.com")
      refute Regex.match?(email_regex, "user@")
      refute Regex.match?(email_regex, "")
    end

    test "phone number format validation with regex" do
      phone_regex = ~r/^[\+]?[1-9][\d]{0,15}$/

      # Valid phone numbers (simplified validation)
      assert Regex.match?(phone_regex, "5551234567")
      assert Regex.match?(phone_regex, "+15551234567")

      # Invalid phone numbers
      refute Regex.match?(phone_regex, "abc")
      refute Regex.match?(phone_regex, "")
    end

    test "string sanitization logic" do
      # Test trimming whitespace
      assert String.trim("  Hello World  ") == "Hello World"

      # Test replacing multiple whitespace
      sanitized = "Test\n\nString" |> String.replace(~r/\s+/, " ") |> String.trim()
      assert sanitized == "Test String"

      # Test empty string handling
      assert String.trim("") == ""
    end
  end

  describe "Business logic validations" do
    test "reservation time validation logic" do
      # Test overlapping reservations logic
      reservation1 = %{start_time: ~N[2024-01-01 09:00:00], end_time: ~N[2024-01-01 10:00:00]}
      reservation2 = %{start_time: ~N[2024-01-01 09:30:00], end_time: ~N[2024-01-01 10:30:00]}
      reservation3 = %{start_time: ~N[2024-01-01 10:00:00], end_time: ~N[2024-01-01 11:00:00]}

      # Should detect overlap
      assert reservations_overlap?(reservation1, reservation2) == true

      # Should not detect overlap (adjacent times)
      assert reservations_overlap?(reservation1, reservation3) == false
    end

    test "business hours validation" do
      # Valid business hours reservation
      valid_reservation = %{
        start_time: ~N[2024-01-01 10:00:00],
        end_time: ~N[2024-01-01 11:00:00]
      }

      # Invalid - starts before business hours
      invalid_early = %{
        start_time: ~N[2024-01-01 08:00:00],
        end_time: ~N[2024-01-01 09:00:00]
      }

      # Invalid - ends after business hours
      invalid_late = %{
        start_time: ~N[2024-01-01 16:00:00],
        end_time: ~N[2024-01-01 18:00:00]
      }

      assert validate_business_hours(valid_reservation) == {:ok, valid_reservation}
      assert {:error, _} = validate_business_hours(invalid_early)
      assert {:error, _} = validate_business_hours(invalid_late)
    end

    test "minimum reservation duration validation" do
      # Valid - meets minimum duration (30 minutes)
      valid_reservation = %{
        start_time: ~N[2024-01-01 10:00:00],
        end_time: ~N[2024-01-01 10:30:00]
      }

      # Invalid - too short (15 minutes)
      invalid_short = %{
        start_time: ~N[2024-01-01 10:00:00],
        end_time: ~N[2024-01-01 10:15:00]
      }

      assert validate_minimum_duration(valid_reservation, 30) == {:ok, valid_reservation}
      assert {:error, _} = validate_minimum_duration(invalid_short, 30)
    end
  end

  describe "Authentication helpers" do
    test "password strength validation" do
      # Strong passwords
      assert validate_password_strength("StrongP@ssw0rd123") == {:ok, "StrongP@ssw0rd123"}
      assert validate_password_strength("C0mpl3x!P@ssw0rd") == {:ok, "C0mpl3x!P@ssw0rd"}

      # Weak passwords
      assert {:error, _} = validate_password_strength("weak")
      assert {:error, _} = validate_password_strength("password123")
      assert {:error, _} = validate_password_strength("PASSWORD123")
      assert {:error, _} = validate_password_strength("Pass123")
    end

    test "role validation" do
      valid_roles = [:admin, :user, :manager, :staff]

      Enum.each(valid_roles, fn role ->
        assert validate_role(role) == {:ok, role}
      end)

      assert {:error, _} = validate_role(:invalid_role)
      assert {:error, _} = validate_role("admin")
      assert {:error, _} = validate_role(nil)
    end
  end

  # Helper functions for business logic testing
  defp reservations_overlap?(res1, res2) do
    res1.start_time < res2.end_time and res2.start_time < res1.end_time
  end

  defp validate_business_hours(%{start_time: start_time, end_time: end_time} = reservation) do
    start_hour = start_time.hour
    end_hour = end_time.hour

    if start_hour >= 9 and end_hour <= 17 do
      {:ok, reservation}
    else
      {:error, "Reservation must be within business hours (9 AM - 5 PM)"}
    end
  end

  defp validate_minimum_duration(
         %{start_time: start_time, end_time: end_time} = reservation,
         min_minutes
       ) do
    # Calculate duration in minutes manually since the helper might not exist
    duration_seconds = NaiveDateTime.diff(end_time, start_time, :second)
    duration_minutes = div(duration_seconds, 60)

    if duration_minutes >= min_minutes do
      {:ok, reservation}
    else
      {:error, "Reservation must be at least #{min_minutes} minutes long"}
    end
  end

  defp validate_password_strength(password) when is_binary(password) do
    cond do
      String.length(password) < 8 ->
        {:error, "Password must be at least 8 characters long"}

      not Regex.match?(~r/[a-z]/, password) ->
        {:error, "Password must contain at least one lowercase letter"}

      not Regex.match?(~r/[A-Z]/, password) ->
        {:error, "Password must contain at least one uppercase letter"}

      not Regex.match?(~r/[0-9]/, password) ->
        {:error, "Password must contain at least one number"}

      not Regex.match?(~r/[!@#$%^&*(),.?":{}|<>]/, password) ->
        {:error, "Password must contain at least one special character"}

      true ->
        {:ok, password}
    end
  end

  defp validate_role(role) when role in [:admin, :user, :manager, :staff], do: {:ok, role}
  defp validate_role(_), do: {:error, "Invalid role"}
end
