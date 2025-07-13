defmodule Storybook.Business do
  use PhoenixStorybook.Index

  def folder_icon, do: {:fa, "building", :thin}
  def folder_name, do: "Business Components"

  def entry("business_card"), do: [name: "Business Card"]
end
