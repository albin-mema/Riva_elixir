defmodule Storybook.Templates do
  use PhoenixStorybook.Index

  def folder_icon, do: {:fa, "folder", :light, "custom-class"}
  def folder_name, do: "Templates"
end
