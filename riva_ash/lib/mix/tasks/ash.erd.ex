defmodule Mix.Tasks.Ash.Erd do
  use Mix.Task
  
  @shortdoc "Generates an ERD diagram for Ash resources"
  
  def run(_args) do
    # Generate the Mermaid diagram
    mermaid_code = RivaAsh.ERD.generate_mermaid(RivaAsh.Domain)
    
    # Print the Mermaid code
    IO.puts(mermaid_code)
    
    # Print instructions for viewing the diagram
    IO.puts("\nTo view the diagram, copy the Mermaid code above and paste it into:")
    IO.puts("https://mermaid.live/")
    
    # Also save to a file for reference
    File.mkdir_p!("priv/static/erd")
    File.write!("priv/static/erd/riva_ash_erd.mmd", mermaid_code)
    IO.puts("\nThe diagram has been saved to: priv/static/erd/riva_ash_erd.mmd")
  end
end
