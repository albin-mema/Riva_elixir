defmodule RivaAsh.Stories.FileUploadStories do
  @moduledoc """
  Storybook stories for FileUpload molecule demonstrating:
  - Basic file selection flow
  - Drag/drop interactions with visual states
  - Loading and error states
  - Integration with FormViewTemplate
  - Mobile viewport behavior
  """
  use ExStorybook.Story, title: "Molecules/FileUpload"

  def basic(assigns) do
    ~H"""
    <div class="p-4 max-w-2xl">
      <RivaAsh.Components.UI.FileUpload id="basic-upload" />
    </div>
    """
  end

  def with_files(assigns) do
    ~H"""
    <div class="p-4 max-w-2xl">
      <RivaAsh.Components.UI.FileUpload
        id="files-upload"
        files={[
          %{id: 1, name: "contract.pdf", progress: 75},
          %{id: 2, name: "profile.jpg", progress: 100},
          %{id: 3, name: "presentation.pptx", progress: 45}
        ]}
      />
    </div>
    """
  end

  def dragging_state(assigns) do
    ~H"""
    <div class="p-4 max-w-2xl">
      <RivaAsh.Components.UI.FileUpload
        id="dragging-upload"
        is_dragging={true}
        is_uploading={false}
      />
    </div>
    """
  end

  def uploading_state(assigns) do
    ~H"""
    <div class="p-4 max-w-2xl">
      <RivaAsh.Components.UI.FileUpload
        id="uploading-upload"
        is_dragging={true}
        is_uploading={true}
        files={[
          %{id: 1, name: "large-video.mp4", progress: 32}
        ]}
      />
    </div>
    """
  end

  def error_state(assigns) do
    ~H"""
    <div class="p-4 max-w-2xl">
      <RivaAsh.Components.UI.FileUpload
        id="error-upload"
        error="File type not allowed. Only PDF and images are supported."
        files={[
          %{id: 1, name: "document.docx", progress: 0}
        ]}
      />
    </div>
    """
  end

  def form_integration(assigns) do
    ~H"""
    <div class="p-4 max-w-2xl">
      <RivaAsh.Components.UI.FormViewTemplate title="Document Upload">
        <:content>
          <RivaAsh.Components.UI.FileUpload
            id="form-upload"
            max_files={3}
            max_size={5_000_000}
            allowed_types={["application/pdf", "image/*"]}
          />
        </:content>
        <:actions>
          <RivaAsh.Components.UI.Button variant="primary">Submit</RivaAsh.Components.UI.Button>
        </:actions>
      </RivaAsh.Components.UI.FormViewTemplate>
    </div>
    """
  end

  def mobile_viewport(assigns) do
    ~H"""
    <div class="p-4 border rounded-lg max-w-xs" style="width: 320px;">
      <RivaAsh.Components.UI.FileUpload
        id="mobile-upload"
        size="sm"
        files={[
          %{id: 1, name: "receipt.jpg", progress: 100}
        ]}
      />
    </div>
    """
  end
end
