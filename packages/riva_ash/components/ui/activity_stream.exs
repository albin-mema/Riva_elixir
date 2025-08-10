defmodule RivaAshWeb.Components.UI.Organisms.ActivityStream do
  use Phoenix.Component
  alias RivaAshWeb.Components.UI.Molecules.{FilterPanel, Pagination, EmptyState}
  alias RivaAshWeb.Components.UI.Atoms.Skeleton

  @doc """
  Activity Stream organism component following Atomic Design composition pattern.
  Composed of: filter_panel + list + pagination + empty_state

  ## Assigns
    * `:activities` - List of activity items with:
        - `:id` - Unique identifier
        - `:type` - Activity type (audit|system|user)
        - `:actor` - Actor name
        - `:action` - Action description
        - `:description` - Detailed description
        - `:timestamp` - ISO8601 timestamp
        - `:timestamp_formatted` - Human-readable timestamp
        - `:tags` - List of activity tags
    * `:loading` - Boolean indicating loading state
    * `:filter_panel` - Assigns for filter panel
    * `:pagination` - Assigns for pagination
    * `:empty_state` - Assigns for empty state
  """
  def activity_stream(assigns) do
    ~H"""
    <div
      role="region"
      aria-label="Activity Stream"
      class="w-full activity-stream"
      phx-hook="ActivityStream"
    >
      <div class="filter-container mb-4">
        <FilterPanel.filter_panel
          {@assigns.filter_panel}
          class="w-full"
          phx-update="ignore"
        />
      </div>

      {#if @loading}
        <div class="skeleton-container" aria-busy="true" aria-live="polite">
          <Skeleton.list
            rows={5}
            class="space-y-3"
            item_class="h-16 rounded-lg"
          />
        </div>
      {:else}
        {#if @activities != []}
          <div
            class="bg-surface border rounded-lg overflow-hidden list-container"
            role="list"
          >
            {#for activity <- @activities}
              <div
                id={"activity-#{activity.id}"}
                role="listitem"
                aria-label={"#{activity.actor} #{activity.action} #{activity.timestamp_formatted}"}
                class={[
                  "activity-item border-b last:border-0 p-4 transition-colors",
                  "min-h-[44px] focus-within:outline focus-within:outline-2 focus-within:outline-offset-2 focus-within:outline-focus",
                  "hover:bg-surface-muted",
                  activity.type == "audit" && "audit-activity",
                  activity.type == "system" && "system-activity",
                  activity.type == "user" && "user-activity"
                ]}
                tabindex="0"
                phx-hook="ActivityItem"
              >
                <div class="md:table w-full">
                  <!-- Mobile card layout -->
                  <div class="md:hidden block">
                    <div class="flex justify-between items-start">
                      <div>
                        <span class="font-medium text-foreground">
                          {activity.actor}
                        </span>
                        <span class="ml-1 text-foreground-muted">
                          {activity.action}
                        </span>
                      </div>
                      <time
                        datetime={activity.timestamp}
                        class="text-foreground-muted text-sm"
                        aria-label={"Timestamp: #{activity.timestamp_formatted}"}
                      >
                        {activity.timestamp_formatted}
                      </time>
                    </div>
                    <p class="mt-1 text-foreground-muted text-sm line-clamp-2">
                      {activity.description}
                    </p>
                    <div class="flex flex-wrap gap-1 mt-2">
                      {#for tag <- activity.tags}
                        <span class={[
                          "inline-flex items-center px-2 py-0.5 rounded text-xs font-medium",
                          "audit" in tag && "bg-audit-surface text-audit-foreground",
                          "system" in tag && "bg-system-surface text-system-foreground",
                          "user" in tag && "bg-user-surface text-user-foreground"
                        ]}>
                          {tag}
                        </span>
                      {/for}
                    </div>
                  </div>

                  <!-- Desktop table layout -->
                  <div class="hidden md:table-row">
                    <div class="table-cell px-4 py-3 w-1/4 align-top">
                      <time
                        datetime={activity.timestamp}
                        class="block text-foreground-muted text-sm"
                        aria-label={"Timestamp: #{activity.timestamp_formatted}"}
                      >
                        {activity.timestamp_formatted}
                      </time>
                    </div>
                    <div class="table-cell px-4 py-3 w-1/4 align-top">
                      <span class="font-medium text-foreground">
                        {activity.actor}
                      </span>
                    </div>
                    <div class="table-cell px-4 py-3 w-1/4 align-top">
                      <span class={[
                        "inline-block px-2 py-1 rounded text-xs font-medium",
                        activity.type == "audit" && "bg-audit-surface text-audit-foreground",
                        activity.type == "system" && "bg-system-surface text-system-foreground",
                        activity.type == "user" && "bg-user-surface text-user-foreground"
                      ]}>
                        {String.capitalize(activity.type)}
                      </span>
                    </div>
                    <div class="table-cell px-4 py-3 w-1/4 align-top">
                      <p class="text-foreground-muted text-sm line-clamp-2">
                        {activity.description}
                      </p>
                      <div class="flex flex-wrap gap-1 mt-1">
                        {#for tag <- activity.tags}
                          <span class="inline-flex items-center bg-tag-surface px-2 py-0.5 rounded font-medium text-tag-foreground text-xs">
                            {tag}
                          </span>
                        {/for}
                      </div>
                    </div>
                  </div>
                </div>
              </div>
            {/for}
          </div>

          <div class="mt-4 pagination-container">
            <Pagination.pagination {@assigns.pagination} />
          </div>
        {:else}
          <div class="empty-container">
            <EmptyState.empty_state {@assigns.empty_state} />
          </div>
        {/if}
      {/if}

      <!-- Screen reader announcement for filter changes -->
      <div
        class="sr-only"
        aria-live="polite"
        aria-atomic="true"
        id="activity-stream-status"
      >
        {#if @loading}
          Loading activity stream
        {:else}
          {Enum.count(@activities)} activities displayed
        {/if}
      </div>
    </div>
    """
  end
end
