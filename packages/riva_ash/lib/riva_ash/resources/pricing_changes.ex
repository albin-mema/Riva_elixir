
defmodule RivaAsh.Resources.Pricing.ApplyPricingValidations do
  @moduledoc false
  use Ash.Resource.Change

  @impl true
  def change(changeset, _opts, _context) do
    changeset
    |> RivaAsh.Resources.Pricing.validate_pricing_date_overlap()
    |> RivaAsh.Resources.Pricing.validate_single_active_base_pricing()
    |> RivaAsh.Resources.Pricing.validate_day_type_pricing()
  end
end

defmodule RivaAsh.Resources.Pricing.ValidateCrossBusinessRelationship do
  @moduledoc false
  use Ash.Resource.Change

  @impl true
  def change(changeset, _opts, _context) do
    RivaAsh.Resources.Pricing.validate_cross_business_relationship(changeset)
  end
end
