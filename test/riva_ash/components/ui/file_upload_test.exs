defmodule RivaAsh.Components.UI.FileUploadTest do
  use RivaAsh.DataCase, async: true
  import Phoenix.LiveViewTest
  import RivaAsh.Components.UI.Atoms

  alias RivaAsh.Components.UI.FileUpload

  describe "basic rendering" do
    test "renders default state with correct ARIA attributes" do
      {:ok, view, _html} = live_isolated(build_conn(), FileUpload, session: %{})

      assert has_element?(view, "[role=button][aria-label='File upload area']")
      assert has_element?(view, "[aria-dropeffect='execute']")
      assert has_element?(view, ".border-dashed")
      refute has_element?(view, ".border-destructive")
    end

    test "renders error state with correct styling" do
      {:ok, view, _html} =
        live_isolated(build_conn(), FileUpload, session: %{}, assigns: %{variant: :error})

      assert has_element?(view, ".border-destructive")
      assert has_element?(view, "[aria-invalid='true']")
    end
  end

  describe "file selection" do
    test "accepts valid files within limits" do
      {:ok, view, _html} = live_isolated(build_conn(), FileUpload, session: %{})

      valid_file = %{
        name: "document.pdf",
        type: "application/pdf",
        size: 5_000_000,
        last_modified: System.system_time(:millisecond)
      }

      view
      |> element("#file-input-file-upload-1")
      |> upload("select_files", [valid_file])

      assert render(view) =~ "document.pdf"
      assert has_element?(view, "[aria-busy='false']")
    end

    test "rejects invalid file types" do
      {:ok, view, _html} = live_isolated(build_conn(), FileUpload, session: %{})

      invalid_file = %{
        name: "document.docx",
        type: "application/vnd.openxmlformats-officedocument.wordprocessingml.document",
        size: 2_000_000,
        last_modified: System.system_time(:millisecond)
      }

      view
      |> element("#file-input-file-upload-1")
      |> upload("select_files", [invalid_file])

      assert render(view) =~ "File type not allowed"
      assert has_element?(view, ".text-destructive")
    end

    test "rejects files exceeding size limit" do
      {:ok, view, _html} =
        live_isolated(build_conn(), FileUpload, session: %{}, assigns: %{max_size: 5_000_000})

      large_file = %{
        name: "large-video.mp4",
        type: "video/mp4",
        size: 15_000_000,
        last_modified: System.system_time(:millisecond)
      }

      view
      |> element("#file-input-file-upload-1")
      |> upload("select_files", [large_file])

      assert render(view) =~ "File size exceeds 5 MB limit"
    end
  end

  describe "drag and drop interactions" do
    test "shows drag state when files are dragged over" do
      {:ok, view, _html} = live_isolated(build_conn(), FileUpload, session: %{})

      view
      |> element("[phx-hook=FileUpload]")
      |> dispatch_event("dragenter", %{})

      assert has_element?(view, ".bg-accent\\/10")
      assert has_element?(view, "[aria-busy='false']")
    end

    test "shows upload state during file processing" do
      {:ok, view, _html} = live_isolated(build_conn(), FileUpload, session: %{})

      view
      |> element("[phx-hook=FileUpload]")
      |> dispatch_event("dragenter", %{})
      |> async_dispatch("drop", %{
        dataTransfer: %{files: [%{name: "image.jpg", type: "image/jpeg", size: 1_000_000}]}
      })

      assert has_element?(view, ".animate-pulse")
      assert has_element?(view, "[aria-busy='true']")
    end
  end

  describe "progress tracking" do
    test "updates progress for uploaded files" do
      {:ok, view, _html} = live_isolated(build_conn(), FileUpload, session: %{})

      valid_file = %{
        name: "document.pdf",
        type: "application/pdf",
        size: 5_000_000,
        last_modified: System.system_time(:millisecond)
      }

      view
      |> element("#file-input-file-upload-1")
      |> upload("select_files", [valid_file])

      # Simulate progress updates
      view
      |> push_event("file_progress", %{id: 1, progress: 50})
      |> push_event("file_progress", %{id: 1, progress: 100})

      assert has_element?(view, "[value='100']")
    end
  end

  describe "error handling" do
    test "displays multiple error messages" do
      {:ok, view, _html} =
        live_isolated(build_conn(), FileUpload, session: %{}, assigns: %{max_files: 2})

      files = [
        %{name: "1.pdf", type: "application/pdf", size: 1_000_000},
        %{name: "2.pdf", type: "application/pdf", size: 1_000_000},
        %{name: "3.pdf", type: "application/pdf", size: 1_000_000}
      ]

      view
      |> element("#file-input-file-upload-1")
      |> upload("select_files", files)

      assert render(view) =~ "Maximum 2 files allowed"
      assert render(view) =~ "3 files selected"
    end

    test "clears error state when valid files are selected" do
      {:ok, view, _html} = live_isolated(build_conn(), FileUpload, session: %{})

      invalid_file = %{name: "doc.docx", type: "application/msword", size: 1_000_000}
      valid_file = %{name: "img.jpg", type: "image/jpeg", size: 1_000_000}

      # First select invalid file
      view
      |> element("#file-input-file-upload-1")
      |> upload("select_files", [invalid_file])

      assert render(view) =~ "File type not allowed"

      # Then select valid file
      view
      |> element("#file-input-file-upload-1")
      |> upload("select_files", [valid_file])

      refute render(view) =~ "File type not allowed"
      assert render(view) =~ "img.jpg"
    end
  end

  describe "accessibility" do
    test "manages ARIA live regions for screen readers" do
      {:ok, view, _html} = live_isolated(build_conn(), FileUpload, session: %{})

      valid_file = %{name: "report.pdf", type: "application/pdf", size: 1_000_000}

      view
      |> element("#file-input-file-upload-1")
      |> upload("select_files", [valid_file])

      assert has_element?(view, "[aria-live='polite']", "File added: report.pdf")
    end

    test "maintains keyboard focus states" do
      {:ok, view, _html} = live_isolated(build_conn(), FileUpload, session: %{})

      assert has_element?(view, "[tabindex='0']")
      assert has_element?(view, ".focus\\:outline-none")
    end
  end
end
