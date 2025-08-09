defmodule RivaAshWeb.Plugs.RateLimiter do
  import Plug.Conn
  alias RivaAsh.Accounts.RateLimiter

  @max_requests 10
  @interval 60 # seconds

  def init(opts), do: opts

  def call(conn, _opts) do
    ip = get_remote_ip(conn)
    user_id = get_user_id(conn)

    case RateLimiter.check_rate(ip, user_id, @max_requests, @interval) do
      {:ok, _count} ->
        conn
        |> put_resp_header("x-ratelimit-limit", to_string(@max_requests))
        |> put_resp_header("x-ratelimit-remaining", to_string(@max_requests - 1))
        |> put_resp_header("x-ratelimit-reset", to_string(@interval))

      {:error, :rate_limited} ->
        conn
        |> put_resp_header("retry-after", to_string(@interval))
        |> send_resp(429, Jason.encode!(%{
          errors: [
            %{
              status: "429",
              title: "Rate Limit Exceeded",
              detail: "Too many requests, please try again later"
            }
          ]
        }))
        |> halt()
    end
  end

  defp get_remote_ip(conn) do
    case conn.remote_ip do
      {a, b, c, d} -> "#{a}.#{b}.#{c}.#{d}"
      ip when is_binary(ip) -> ip
      _ -> "unknown"
    end
  end

  defp get_user_id(conn) do
    # Try to get user ID from authenticated session
    case get_session(conn, :user_id) do
      nil ->
        # Try to get from token if available
        case get_req_header(conn, "authorization") do
          ["Bearer " <> token] ->
            case RivaAsh.Accounts.get_user_by_token(token) do
              {:ok, user} -> user.id
              _ -> nil
            end
          _ -> nil
        end
      user_id -> user_id
    end
  end
end
