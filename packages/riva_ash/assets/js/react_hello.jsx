import React from "react";

export default function ReactHello(props) {
  return (
    <div data-testid="react-hello" style={{ padding: 8, border: "1px dashed #888" }}>
      Hello from React! {props.name ? `Name: ${props.name}` : null}
    </div>
  );
}

// Register globally for LiveReact
window.ReactHello = ReactHello;

