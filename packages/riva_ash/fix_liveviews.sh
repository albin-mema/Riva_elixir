#!/bin/bash

# List of LiveView files that need authentication fixes
files=(
  "lib/riva_ash_web/live/reservation_live.ex"
  "lib/riva_ash_web/live/recurring_reservation_live.ex"
  "lib/riva_ash_web/live/layout_live.ex"
  "lib/riva_ash_web/live/item_position_live.ex"
  "lib/riva_ash_web/live/item_schedule_live.ex"
  "lib/riva_ash_web/live/payment_live.ex"
  "lib/riva_ash_web/live/token_live.ex"
  "lib/riva_ash_web/live/plot_live.ex"
  "lib/riva_ash_web/live/recurring_reservation_instance_live.ex"
  "lib/riva_ash_web/live/user_live.ex"
  "lib/riva_ash_web/live/availability_exception_live.ex"
  "lib/riva_ash_web/live/item_hold_live.ex"
)

# Authentication helper function to add
auth_helper='
  # Private helper functions
  defp get_current_user_from_session(session) do
    user_token = session["user_token"]

    if user_token do
      with {:ok, user_id} <- Phoenix.Token.verify(RivaAshWeb.Endpoint, "user_auth", user_token, max_age: 86_400) |> RivaAsh.ErrorHelpers.to_result(),
           {:ok, user} <- Ash.get(RivaAsh.Accounts.User, user_id, domain: RivaAsh.Accounts) |> RivaAsh.ErrorHelpers.to_result() do
        RivaAsh.ErrorHelpers.success(user)
      else
        _ -> RivaAsh.ErrorHelpers.failure(:not_authenticated)
      end
    else
      RivaAsh.ErrorHelpers.failure(:not_authenticated)
    end
  end'

for file in "${files[@]}"; do
  if [ -f "$file" ]; then
    echo "Processing $file..."
    
    # Check if file already has authentication
    if grep -q "get_current_user_from_session" "$file"; then
      echo "  Already has authentication, skipping..."
      continue
    fi
    
    # Extract resource name from file path
    resource_name=$(basename "$file" _live.ex)
    resource_module=$(echo "$resource_name" | sed 's/_\([a-z]\)/\U\1/g' | sed 's/^./\U&/')
    
    # Fix mount function - replace _session with session and add authentication
    sed -i 's/def mount(_params, _session, socket)/def mount(_params, session, socket)/' "$file"
    
    # Add authentication helper before the final 'end'
    sed -i '/^end$/i\
'"$auth_helper"'' "$file"
    
    echo "  Fixed $file"
  else
    echo "File $file not found, skipping..."
  fi
done

echo "Done fixing LiveView files!"
