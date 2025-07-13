defmodule Storybook.Molecules do
  use PhoenixStorybook.Index

  def folder_icon, do: {:fa, "cubes", :thin}
  def folder_name, do: "Molecules"

  def entry("card"), do: [name: "Card"]
end
