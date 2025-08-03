defmodule RivaAshWeb.UIDemoController do
  use RivaAshWeb, :controller

  def index(conn, _params) do
    render(conn, :index)
  end
end
