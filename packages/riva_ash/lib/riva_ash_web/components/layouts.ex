defmodule RivaAshWeb.Layouts do
  @moduledoc """
  This module contains different layouts used by your application.

  See the `layouts` directory for all templates available.
  The "root" layout is a skeleton rendered around other layouts.
  The "app" layout is the default layout used by most pages.
  """
  use RivaAshWeb, :html

  import RivaAshWeb.CoreComponents

  embed_templates "core/layouts/*"

  def root(assigns) do
    ~H"""
    <!DOCTYPE html>
    <html lang="en">
      <head>
        <meta charset="utf-8"/>
        <meta name="viewport" content="width=device-width, initial-scale=1"/>
        <meta name="csrf-token" content={Phoenix.Controller.get_csrf_token()}/>
        <%= live_title_tag assigns[:page_title] || "Riva", suffix: " · Phoenix Framework" %>
        <link phx-track-static rel="stylesheet" href={~p"/assets/app.css"}/>
        <script defer phx-track-static type="text/javascript" src={~p"/assets/app.js"}></script>
      </head>
      <body class="bg-gray-100">
        <%= @inner_content %>
      </body>
    </html>
    """
  end
end
