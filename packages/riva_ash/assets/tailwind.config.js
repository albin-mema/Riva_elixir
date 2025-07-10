/** @type {import('tailwindcss').Config} */
module.exports = {
  content: [
    "../deps/salad_ui/lib/**/*.ex",
    "../lib/**/*.{ex,heex,eex}",
    "../lib/**/*.{js,jsx,ts,tsx}",
    "./js/**/*.{js,jsx,ts,tsx}",
    "../priv/static/**/*.{js,css}"
  ],
  theme: {
    extend: {
      colors: require("./tailwind.colors.json"),
    },
  },
  plugins: [],
}
