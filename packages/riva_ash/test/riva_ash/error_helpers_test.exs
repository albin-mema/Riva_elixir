defmodule RivaAsh.ErrorHelpersTest do
  use ExUnit.Case, async: true
  alias RivaAsh.ErrorHelpers

  @moduletag :unit
  @moduletag :pure
  @moduletag :fast

  describe "translate_error/1" do
    test "translates Ecto changeset errors" do
      changeset = %Ecto.Changeset{
        errors: [name: {"can't be blank", [validation: :required]}],
        data: %{},
        types: %{name: :string}
      }

      result = ErrorHelpers.translate_error({:name, {"can't be blank", [validation: :required]}})
      assert is_binary(result)
    end

    test "handles custom error messages" do
      result = ErrorHelpers.translate_error({:field, {"custom error", []}})
      assert is_binary(result)
    end
  end

  describe "format_changeset_errors/1" do
    test "formats changeset errors into readable messages" do
      changeset = %Ecto.Changeset{
        errors: [
          name: {"can't be blank", [validation: :required]},
          email: {"has invalid format", [validation: :format]}
        ],
        data: %{},
        types: %{name: :string, email: :string}
      }

      result = ErrorHelpers.format_changeset_errors(changeset)
      assert is_list(result)
      assert length(result) == 2
    end

    test "handles empty changeset errors" do
      changeset = %Ecto.Changeset{
        errors: [],
        data: %{},
        types: %{}
      }

      result = ErrorHelpers.format_changeset_errors(changeset)
      assert result == []
    end
  end

  describe "error_to_string/1" do
    test "converts error tuples to strings" do
      assert ErrorHelpers.error_to_string({:error, "Something went wrong"}) == "Something went wrong"
      assert ErrorHelpers.error_to_string({:error, :not_found}) == "not found"
      assert ErrorHelpers.error_to_string({:error, :invalid_input}) == "invalid input"
    end

    test "handles string errors directly" do
      assert ErrorHelpers.error_to_string("Error message") == "Error message"
    end

    test "handles atom errors" do
      assert ErrorHelpers.error_to_string(:timeout) == "timeout"
      assert ErrorHelpers.error_to_string(:unauthorized) == "unauthorized"
    end
  end

  describe "log_error/2" do
    test "logs error with context" do
      assert :ok = ErrorHelpers.log_error("Test error", %{context: "test"})
    end

    test "handles nil context" do
      assert :ok = ErrorHelpers.log_error("Test error", nil)
    end
  end

  describe "wrap_error/2" do
    test "wraps error with additional context" do
      result = ErrorHelpers.wrap_error({:error, :not_found}, "User lookup failed")
      assert result == {:error, "User lookup failed: not found"}
    end

    test "handles string errors" do
      result = ErrorHelpers.wrap_error({:error, "Original error"}, "Additional context")
      assert result == {:error, "Additional context: Original error"}
    end
  end

  describe "handle_database_error/1" do
    test "handles constraint violation errors" do
      error = %Postgrex.Error{
        postgres: %{
          code: :unique_violation,
          message: "duplicate key value violates unique constraint"
        }
      }

      result = ErrorHelpers.handle_database_error(error)
      assert result == {:error, "This record already exists"}
    end

    test "handles foreign key violations" do
      error = %Postgrex.Error{
        postgres: %{
          code: :foreign_key_violation,
          message: "insert or update on table violates foreign key constraint"
        }
      }

      result = ErrorHelpers.handle_database_error(error)
      assert result == {:error, "Referenced record does not exist"}
    end

    test "handles generic database errors" do
      error = %Postgrex.Error{
        postgres: %{
          code: :undefined_table,
          message: "relation does not exist"
        }
      }

      result = ErrorHelpers.handle_database_error(error)
      assert result == {:error, "Database error occurred"}
    end
  end

  describe "validation_errors_to_map/1" do
    test "converts changeset errors to map" do
      changeset = %Ecto.Changeset{
        errors: [
          name: {"can't be blank", [validation: :required]},
          email: {"has invalid format", [validation: :format]}
        ],
        data: %{},
        types: %{name: :string, email: :string}
      }

      result = ErrorHelpers.validation_errors_to_map(changeset)
      assert is_map(result)
      assert Map.has_key?(result, :name)
      assert Map.has_key?(result, :email)
    end
  end
end
