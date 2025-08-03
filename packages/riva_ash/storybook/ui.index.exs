defmodule Storybook.UI do
  use PhoenixStorybook.Index

  def folder_icon, do: {:fa, "folder", :light, "custom-class"}
  def folder_name, do: "UI Components"

  def entry("button"), do: [name: "Button", icon: {:fa, "square", :light}]
  def entry("input"), do: [name: "Input", icon: {:fa, "font", :light}]
  def entry("checkbox"), do: [name: "Checkbox", icon: {:fa, "check-square", :light}]
  def entry("select"), do: [name: "Select", icon: {:fa, "list", :light}]
  def entry("textarea"), do: [name: "Textarea", icon: {:fa, "align-left", :light}]
  def entry("badge"), do: [name: "Badge", icon: {:fa, "tag", :light}]
  def entry("card"), do: [name: "Card", icon: {:fa, "square", :light}]
  def entry("alert"), do: [name: "Alert", icon: {:fa, "exclamation-triangle", :light}]
end
