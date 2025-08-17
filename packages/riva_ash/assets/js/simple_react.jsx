import React, { useState } from "react";

export function SimpleReact({ label = "Hello from React", count = 0, livePush }) {
  const [localCount, setLocalCount] = useState(count);

  function handleClick() {
    const next = localCount + 1;
    setLocalCount(next);
    livePush && livePush("simple_clicked", { count: next });
  }

  return (
    <button
      type="button"
      className="px-3 py-2 rounded bg-blue-600 text-white hover:bg-blue-700"
      onClick={handleClick}
    >
      {label} ({localCount})
    </button>
  );
}

// Expose globally for LiveReact
window.SimpleReact = SimpleReact;

