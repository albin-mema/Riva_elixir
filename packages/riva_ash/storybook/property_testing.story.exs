defmodule Storybook.PropertyTesting do
  @moduledoc """
  Property-based testing showcase for Storybook components.
  
  This story demonstrates how property-based testing can generate
  hundreds of component variations automatically, helping discover
  edge cases and ensuring robust component behavior.
  """
  
  use PhoenixStorybook.Story, :page
  
  import RivaAsh.StorybookTesting.PropertyGenerators
  
  def doc do
    """
    Property-based testing for UI components using StreamData.
    
    This page showcases automatically generated component variations
    that help discover edge cases and ensure component robustness.
    """
  end
  
  def navigation do
    [
      {:overview, "Overview", {:fa, "flask", :thin}},
      {:button_variations, "Button Variations", {:fa, "square", :thin}},
      {:input_variations, "Input Variations", {:fa, "text-cursor", :thin}},
      {:edge_cases, "Edge Cases", {:fa, "triangle-exclamation", :thin}},
      {:performance, "Performance", {:fa, "gauge-high", :thin}}
    ]
  end
  
  def render(assigns = %{tab: :overview}) do
    ~H"""
    <div class="psb-welcome-page">
      <h1 class="text-3xl font-bold mb-6">Property-Based Testing for Storybook</h1>
      
      <div class="bg-blue-50 border border-blue-200 rounded-lg p-6 mb-8">
        <h2 class="text-xl font-semibold mb-3 text-blue-800">🚀 What is Property-Based Testing?</h2>
        <p class="text-blue-700 mb-4">
          Property-based testing automatically generates hundreds or thousands of test cases
          with random data to verify that your components behave correctly under all conditions.
        </p>
        <ul class="list-disc list-inside text-blue-700 space-y-2">
          <li><strong>Automatic Test Generation:</strong> No need to manually write test cases</li>
          <li><strong>Edge Case Discovery:</strong> Finds bugs you never thought to test</li>
          <li><strong>Comprehensive Coverage:</strong> Tests with extreme values, empty strings, special characters</li>
          <li><strong>Regression Prevention:</strong> Ensures components work with any valid input</li>
        </ul>
      </div>
      
      <div class="grid grid-cols-1 md:grid-cols-2 gap-6 mb-8">
        <div class="bg-green-50 border border-green-200 rounded-lg p-6">
          <h3 class="text-lg font-semibold mb-3 text-green-800">✅ Benefits</h3>
          <ul class="list-disc list-inside text-green-700 space-y-1">
            <li>Finds edge cases automatically</li>
            <li>Reduces manual test writing</li>
            <li>Improves component reliability</li>
            <li>Catches XSS vulnerabilities</li>
            <li>Validates accessibility</li>
          </ul>
        </div>
        
        <div class="bg-purple-50 border border-purple-200 rounded-lg p-6">
          <h3 class="text-lg font-semibold mb-3 text-purple-800">🔧 Implementation</h3>
          <ul class="list-disc list-inside text-purple-700 space-y-1">
            <li>StreamData for random generation</li>
            <li>Phoenix component testing</li>
            <li>Browser automation with Playwright</li>
            <li>Visual regression testing</li>
            <li>Performance benchmarking</li>
          </ul>
        </div>
      </div>
      
      <div class="bg-yellow-50 border border-yellow-200 rounded-lg p-6">
        <h3 class="text-lg font-semibold mb-3 text-yellow-800">⚡ Quick Start</h3>
        <pre class="bg-gray-800 text-green-400 p-4 rounded text-sm overflow-x-auto"><code># Run property-based tests
mix test test/riva_ash_web/storybook_property_test.exs

# Run with more iterations
PROPERTY_MAX_RUNS=1000 mix test test/riva_ash_web/storybook_property_test.exs

# Run visual regression tests
PROPERTY_VISUAL=true mix test test/riva_ash_web/storybook_property_test.exs --include visual</code></pre>
      </div>
    </div>
    """
  end
  
  def render(assigns = %{tab: :button_variations}) do
    # Generate 20 random button variations
    button_variations = 
      button_props()
      |> Enum.take(20)
      |> Enum.with_index()
    
    assigns = assign(assigns, :button_variations, button_variations)
    
    ~H"""
    <div class="psb-welcome-page">
      <h1 class="text-3xl font-bold mb-6">🔘 Button Property Variations</h1>
      
      <p class="text-gray-600 mb-8">
        These 20 button variations were automatically generated using property-based testing.
        Each refresh shows different random combinations of props.
      </p>
      
      <div class="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-6">
        <%= for {props, index} <- @button_variations do %>
          <div class="border border-gray-200 rounded-lg p-4 bg-white">
            <h3 class="text-sm font-semibold mb-3 text-gray-700">Variation <%= index + 1 %></h3>
            
            <!-- Render the button with generated props -->
            <div class="mb-4 flex justify-center">
              <.button 
                variant={props.variant}
                size={props.size}
                disabled={props.disabled}
                loading={props.loading}
                type={props.type}
                class={props.class}
              >
                <%= props.text %>
              </.button>
            </div>
            
            <!-- Show the generated props -->
            <details class="text-xs">
              <summary class="cursor-pointer text-blue-600 hover:text-blue-800">View Props</summary>
              <pre class="mt-2 bg-gray-100 p-2 rounded text-xs overflow-x-auto"><%= inspect(props, pretty: true) %></pre>
            </details>
          </div>
        <% end %>
      </div>
      
      <div class="mt-8 bg-blue-50 border border-blue-200 rounded-lg p-6">
        <h3 class="text-lg font-semibold mb-3 text-blue-800">🧪 Testing Properties</h3>
        <p class="text-blue-700 mb-4">Each button variation tests:</p>
        <ul class="list-disc list-inside text-blue-700 space-y-1">
          <li><strong>Rendering:</strong> Component renders without crashing</li>
          <li><strong>Props:</strong> All prop combinations work correctly</li>
          <li><strong>HTML:</strong> Valid HTML structure is maintained</li>
          <li><strong>Security:</strong> No XSS vulnerabilities with user content</li>
          <li><strong>Accessibility:</strong> Proper button semantics preserved</li>
        </ul>
      </div>
    </div>
    """
  end
  
  def render(assigns = %{tab: :input_variations}) do
    # Generate 15 random input variations
    input_variations = 
      input_props()
      |> Enum.take(15)
      |> Enum.with_index()
    
    assigns = assign(assigns, :input_variations, input_variations)
    
    ~H"""
    <div class="psb-welcome-page">
      <h1 class="text-3xl font-bold mb-6">📝 Input Property Variations</h1>
      
      <p class="text-gray-600 mb-8">
        These input variations demonstrate different types, states, and edge cases
        that property-based testing automatically discovers.
      </p>
      
      <div class="grid grid-cols-1 md:grid-cols-2 gap-6">
        <%= for {props, index} <- @input_variations do %>
          <div class="border border-gray-200 rounded-lg p-4 bg-white">
            <h3 class="text-sm font-semibold mb-3 text-gray-700">Input <%= index + 1 %></h3>
            
            <!-- Render the input with generated props -->
            <div class="mb-4">
              <.input 
                type={props.type}
                placeholder={props.placeholder}
                value={props.value}
                disabled={props.disabled}
                readonly={props.readonly}
                required={props.required}
                size={props.size}
                variant={props.variant}
                class={props.class}
              />
            </div>
            
            <!-- Show the generated props -->
            <details class="text-xs">
              <summary class="cursor-pointer text-blue-600 hover:text-blue-800">View Props</summary>
              <pre class="mt-2 bg-gray-100 p-2 rounded text-xs overflow-x-auto"><%= inspect(props, pretty: true) %></pre>
            </details>
          </div>
        <% end %>
      </div>
    </div>
    """
  end
  
  def render(assigns = %{tab: :edge_cases}) do
    # Generate edge case examples
    button_edge_cases = edge_case_props(:button) |> Enum.take(5)
    input_edge_cases = edge_case_props(:input) |> Enum.take(5)
    
    assigns = assign(assigns, :button_edge_cases, button_edge_cases)
    assigns = assign(assigns, :input_edge_cases, input_edge_cases)
    
    ~H"""
    <div class="psb-welcome-page">
      <h1 class="text-3xl font-bold mb-6">⚠️ Edge Case Testing</h1>
      
      <p class="text-gray-600 mb-8">
        Property-based testing excels at finding edge cases that break components.
        Here are some examples of extreme inputs that your components should handle gracefully.
      </p>
      
      <div class="space-y-8">
        <div>
          <h2 class="text-xl font-semibold mb-4">🔘 Button Edge Cases</h2>
          <div class="grid grid-cols-1 md:grid-cols-2 gap-4">
            <%= for {props, index} <- Enum.with_index(@button_edge_cases) do %>
              <div class="border border-red-200 rounded-lg p-4 bg-red-50">
                <h3 class="text-sm font-semibold mb-3 text-red-700">Edge Case <%= index + 1 %></h3>
                
                <div class="mb-4 flex justify-center">
                  <.button variant={props.variant || "primary"}>
                    <%= props.text || "Button" %>
                  </.button>
                </div>
                
                <details class="text-xs">
                  <summary class="cursor-pointer text-red-600 hover:text-red-800">View Props</summary>
                  <pre class="mt-2 bg-white p-2 rounded text-xs overflow-x-auto"><%= inspect(props, pretty: true) %></pre>
                </details>
              </div>
            <% end %>
          </div>
        </div>
        
        <div>
          <h2 class="text-xl font-semibold mb-4">📝 Input Edge Cases</h2>
          <div class="grid grid-cols-1 md:grid-cols-2 gap-4">
            <%= for {props, index} <- Enum.with_index(@input_edge_cases) do %>
              <div class="border border-red-200 rounded-lg p-4 bg-red-50">
                <h3 class="text-sm font-semibold mb-3 text-red-700">Edge Case <%= index + 1 %></h3>
                
                <div class="mb-4">
                  <.input 
                    type={props.type || "text"}
                    value={props.value}
                    placeholder="Edge case input"
                  />
                </div>
                
                <details class="text-xs">
                  <summary class="cursor-pointer text-red-600 hover:text-red-800">View Props</summary>
                  <pre class="mt-2 bg-white p-2 rounded text-xs overflow-x-auto"><%= inspect(props, pretty: true) %></pre>
                </details>
              </div>
            <% end %>
          </div>
        </div>
      </div>
      
      <div class="mt-8 bg-red-50 border border-red-200 rounded-lg p-6">
        <h3 class="text-lg font-semibold mb-3 text-red-800">🛡️ Security & Robustness</h3>
        <p class="text-red-700 mb-4">Edge case testing helps ensure:</p>
        <ul class="list-disc list-inside text-red-700 space-y-1">
          <li><strong>XSS Prevention:</strong> Malicious scripts are properly escaped</li>
          <li><strong>Input Validation:</strong> Invalid data doesn't break the UI</li>
          <li><strong>Performance:</strong> Large inputs don't cause performance issues</li>
          <li><strong>Accessibility:</strong> Screen readers work with edge case content</li>
          <li><strong>Graceful Degradation:</strong> Components fail safely</li>
        </ul>
      </div>
    </div>
    """
  end
  
  def render(assigns = %{tab: :performance}) do
    ~H"""
    <div class="psb-welcome-page">
      <h1 class="text-3xl font-bold mb-6">⚡ Performance Testing</h1>
      
      <p class="text-gray-600 mb-8">
        Property-based testing can also validate performance characteristics
        by rendering many component variations and measuring timing.
      </p>
      
      <div class="grid grid-cols-1 md:grid-cols-2 gap-6 mb-8">
        <div class="bg-green-50 border border-green-200 rounded-lg p-6">
          <h3 class="text-lg font-semibold mb-3 text-green-800">📊 Benchmarks</h3>
          <ul class="list-disc list-inside text-green-700 space-y-2">
            <li>50 button components: &lt;100ms</li>
            <li>100 input components: &lt;200ms</li>
            <li>Complex forms: &lt;500ms</li>
            <li>Memory usage: &lt;10MB</li>
          </ul>
        </div>
        
        <div class="bg-blue-50 border border-blue-200 rounded-lg p-6">
          <h3 class="text-lg font-semibold mb-3 text-blue-800">🔍 What We Test</h3>
          <ul class="list-disc list-inside text-blue-700 space-y-2">
            <li>Component render time</li>
            <li>Memory allocation</li>
            <li>DOM node count</li>
            <li>CSS computation time</li>
          </ul>
        </div>
      </div>
      
      <div class="bg-yellow-50 border border-yellow-200 rounded-lg p-6">
        <h3 class="text-lg font-semibold mb-3 text-yellow-800">🚀 Run Performance Tests</h3>
        <pre class="bg-gray-800 text-green-400 p-4 rounded text-sm overflow-x-auto"><code># Run performance property tests
mix test test/riva_ash_web/storybook_property_test.exs -t performance

# Run with more iterations for better statistics
PROPERTY_MAX_RUNS=500 mix test test/riva_ash_web/storybook_property_test.exs -t performance

# Profile memory usage
mix test test/riva_ash_web/storybook_property_test.exs -t memory_profile</code></pre>
      </div>
    </div>
    """
  end
end
