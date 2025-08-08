defmodule Storybook.Molecules do
  use PhoenixStorybook.Index

  def folder_icon, do: {:fa, "cubes", :thin}
  def folder_name, do: "Molecules"

  def entry("action_menu"), do: [name: "Action Menu"]
  def entry("breadcrumb_nav"), do: [name: "Breadcrumb Nav"]
  def entry("card"), do: [name: "Card"]
  def entry("chat_messages_list"), do: [name: "Chat Messages List"]
  def entry("chat_messages_list"), do: [name: "Chat Messages List"]
  def entry("confirm_dialog"), do: [name: "Confirm Dialog"]
  def entry("empty_state"), do: [name: "Empty State"]
  def entry("filter_panel"), do: [name: "Filter Panel"]
  def entry("form_field"), do: [name: "Form Field"]
  def entry("notification_toast"), do: [name: "Notification Toast"]
  def entry("pagination"), do: [name: "Pagination"]
  def entry("progress_bar"), do: [name: "Progress Bar"]
  def entry("search_bar"), do: [name: "Search Bar"]
  def entry("status_indicator"), do: [name: "Status Indicator"]
  def entry("tab_navigation"), do: [name: "Tab Navigation"]
end
