/** @type {import('tailwindcss').Config} */
module.exports = {
  important: ".riva-ash-web",
  content: [
    "../deps/salad_ui/lib/**/*.ex",
    "../lib/**/*.{ex,heex,eex}",
    "../lib/**/*.{js,jsx,ts,tsx}",
    "./js/**/*.{js,jsx,ts,tsx}",
    "../priv/static/**/*.{js,css}",
    "../storybook/**/*.exs"
  ],
  theme: {
    extend: {
      colors: require("./tailwind.colors.json"),
    },
  },
  plugins: [],
}
