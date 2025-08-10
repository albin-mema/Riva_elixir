defmodule RivaAsh.Stories.UI.TwoPaneTemplate do
  use Phoenix.Component
  use RivaAsh.Storybook

  def default(assigns) do
    ~H"""
    <TwoPaneTemplate title="Dashboard" sidebar_open={true}>
      <:sidebar>
        <ul class="space-y-2">
          <li><a href="#" class="text-blue-600">Home</a></li>
          <li><a href="#" class="text-blue-600">Settings</a></li>
        </ul>
      </:sidebar>
      <:main>
        <div class="p-4">
          <h2>Welcome to the Dashboard</h2>
          <p>Main content goes here</p>
        </div>
      </:main>
    </TwoPaneTemplate>
    """
  end

  def with_all_slots(assigns) do
    ~H"""
    <TwoPaneTemplate title="User Profile" sidebar_open={true}>
      <:breadcrumbs>
        <span class="text-gray-500">Home / Profile</span>
      </:breadcrumbs>
      <:toolbar>
        <button class="bg-gray-200 px-3 py-1">Edit</button>
      </:toolbar>
      <:actions>
        <button class="bg-green-500 px-4 py-2 text-white">Save Changes</button>
      </:actions>
      <:tabs>
        <div class="flex space-x-4">
          <a href="#" class="font-bold text-blue-600">Overview</a>
          <a href="#" class="text-blue-600">Activity</a>
        </div>
      </:tabs>
      <:main>
        <div class="p-4">
          <h2>User Profile</h2>
          <p>Detailed user information here</p>
        </div>
      </:main>
    </TwoPaneTemplate>
    """
  end

  def loading_state(assigns) do
    ~H"""
    <TwoPaneTemplate title="Loading..." loading={true}>
      <:sidebar>
        <ul class="space-y-2">
          <li><span class="animate-pulse">Sidebar loading</span></li>
        </ul>
      </:sidebar>
      <:main>
        <div class="p-4">
          <h2>Loading content</h2>
          <p>Please wait while data is being fetched</p>
        </div>
      </:main>
    </TwoPaneTemplate>
    """
  end
end
