defmodule Storybook.Atoms do
  use PhoenixStorybook.Index

  def folder_icon, do: {:fa, "atom", :thin}
  def folder_name, do: "Atoms"

  def entry("avatar"), do: [name: "Avatar"]
  def entry("badge"), do: [name: "Badge"]
  def entry("button"), do: [name: "Button"]
  def entry("checkbox"), do: [name: "Checkbox"]
  def entry("chat_input"), do: [name: "Chat Input"]
  def entry("chat_message"), do: [name: "Chat Message"]
  def entry("date_picker"), do: [name: "Date Picker"]
  def entry("icon"), do: [name: "Icon"]
  def entry("input"), do: [name: "Input"]
  def entry("radio"), do: [name: "Radio"]
  def entry("room_list_item"), do: [name: "Room List Item"]
  def entry("select"), do: [name: "Select"]
  def entry("spinner"), do: [name: "Spinner"]
  def entry("text"), do: [name: "Text"]
  def entry("time_picker"), do: [name: "Time Picker"]
  def entry("toggle"), do: [name: "Toggle"]
  def entry("tooltip"), do: [name: "Tooltip"]
end
