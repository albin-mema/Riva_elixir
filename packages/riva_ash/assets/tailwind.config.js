/** @type {import('tailwindcss').Config} */
module.exports = {
  content: [
    "../lib/**/*.{ex,heex,eex}",
    "../lib/**/*.{js,jsx,ts,tsx}",
    "./js/**/*.{js,jsx,ts,tsx}",
    "../priv/static/**/*.{js,css}"
  ],
  theme: {
    extend: {},
  },
  plugins: [],
}
