defmodule RivaAshWeb.ReactButtonController do
  use RivaAshWeb, :controller

  def show(conn, _params) do
    render(conn, :show, layout: false)
  end
end

