defmodule RivaAshWeb.Components.UI.AtomsTest do
  use RivaAshWeb.ConnCase, async: true
  import Phoenix.Component
  import Phoenix.LiveViewTest

  alias RivaAsh.Components.UI.Atoms

  describe "standard_assigns/2" do
    test "applies default variant when none provided" do
      assigns = Atoms.standard_assigns(%{})
      assert assigns.variant == :primary
    end

    test "validates variant options" do
      assigns = Atoms.standard_assigns(%{variant: :invalid})
      assert assigns.variant == :primary

      assigns = Atoms.standard_assigns(%{variant: :secondary})
      assert assigns.variant == :secondary
    end

    test "applies default size when none provided" do
      assigns = Atoms.standard_assigns(%{})
      assert assigns.size == :md
    end

    test "handles disabled state" do
      assigns = Atoms.standard_assigns(%{disabled: true})
      assert assigns.disabled == true
    end

    test "preserves aria_label" do
      assigns = Atoms.standard_assigns(%{aria_label: "Submit"})
      assert assigns.aria_label == "Submit"
    end
  end

  describe "variant_classes/2" do
    test "generates primary button classes" do
      assert Atoms.variant_classes(:button, :primary) ==
               "bg-primary text-primary-foreground hover:bg-primary/90 active:bg-primary/80"
    end

    test "generates secondary button classes" do
      assert Atoms.variant_classes(:button, :secondary) ==
               "bg-secondary text-secondary-foreground hover:bg-secondary/90 active:bg-secondary/80"
    end

    test "generates tertiary button classes" do
      assert Atoms.variant_classes(:button, :tertiary) ==
               "bg-accent text-accent-foreground hover:bg-accent/90 active:bg-accent/80"
    end
  end

  describe "size_classes/2" do
    test "generates small button classes" do
      assert Atoms.size_classes(:button, :sm) == "px-2 py-1 text-xs"
    end

    test "generates medium button classes" do
      assert Atoms.size_classes(:button, :md) == "px-4 py-2 text-sm"
    end

    test "generates large button classes" do
      assert Atoms.size_classes(:button, :lg) == "px-6 py-3 text-base"
    end
  end

  describe "focus_styles/0" do
    test "generates proper focus styles" do
      assert Atoms.focus_styles() ==
               "focus:outline-none focus:ring-2 focus:ring-ring focus:ring-offset-2"
    end
  end

  describe "aria_attributes/1" do
    test "generates aria-disabled when disabled" do
      assigns = %{disabled: true, aria_label: "Submit"}
      assert Atoms.aria_attributes(assigns) == %{"aria-disabled" => true, "aria-label" => "Submit"}
    end

    test "omits false attributes" do
      assigns = %{disabled: false, aria_label: nil}
      assert Atoms.aria_attributes(assigns) == %{}
    end
  end
end
