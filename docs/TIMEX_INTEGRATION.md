# Timex Integration Documentation

## Overview

This project has been successfully integrated with [Timex](https://github.com/bitwalker/timex), the most comprehensive date/time library for Elixir. Timex provides powerful date/time manipulation, timezone support, and formatting capabilities that enhance the project's date/time handling.

## Why Timex?

Timex was chosen as the best date/time library for Elixir based on:

- **Most Popular**: 1.7k+ stars, 126M+ total downloads
- **Comprehensive**: Rich, full-featured Date/Time library with timezone support
- **Active Maintenance**: Recently updated (March 2024) and well-maintained
- **Full Timezone Support**: Complete timezone support via the `:tzdata` package
- **Widely Adopted**: Used by major Elixir packages including the Ash framework
- **Extensive API**: Provides parsing, formatting, arithmetic, comparisons, and more

## Installation

Timex has been added to the project dependencies in `mix.exs`:

```elixir
{:timex, "~> 3.7"}
```

## Files Modified

The following files have been updated to use Timex instead of standard Elixir DateTime/Date/Time functions:

### Core Business Logic

1. **`lib/riva_ash/availability.ex`**
   - Replaced `Date.day_of_week/1` with `Timex.weekday/1`
   - Replaced `DateTime.diff/3` with `Timex.diff/3`
   - Replaced `DateTime.to_date/1` with `Timex.to_date/1`
   - Replaced `Date.range/2` with `Timex.Interval` operations
   - Replaced `DateTime.compare/2` with `Timex.compare/2`
   - Replaced `Time.add/3` with `Timex.shift/2`

2. **`lib/riva_ash/booking.ex`**
   - Replaced `DateTime.utc_now/0` with `Timex.now/0`
   - Replaced `DateTime.diff/3` with `Timex.diff/3`
   - Replaced `DateTime.add/3` with `Timex.shift/2`
   - Replaced `DateTime.compare/2` with `Timex.compare/2`

3. **`lib/riva_ash/recurring_reservations.ex`**
   - Replaced `DateTime.utc_now/0` with `Timex.now/0`
   - Replaced `Date.add/2` with `Timex.shift/2`
   - Replaced `Date.day_of_week/1` with `Timex.weekday/1`
   - Replaced `DateTime.compare/2` with `Timex.compare/2`

### Test Infrastructure

4. **`test/support/factory.ex`**
   - Replaced `DateTime.utc_now/0` with `Timex.now/0`
   - Replaced `DateTime.add/3` with `Timex.shift/2`
   - Replaced `DateTime.compare/2` with `Timex.compare/2`

5. **`test/support/property_helpers.ex`**
   - Replaced `DateTime.add/3` with `Timex.shift/2`
   - Replaced `DateTime.utc_now/0` with `Timex.now/0`

## Key Improvements

### Enhanced Date/Time Operations

- **Better Date Arithmetic**: `Timex.shift/2` provides more intuitive date/time shifting
- **Improved Comparisons**: `Timex.compare/2` returns -1, 0, 1 for cleaner logic
- **Timezone Awareness**: Built-in timezone support for global applications
- **Flexible Formatting**: Rich formatting options with `Timex.format!/2`

### Specific Enhancements

1. **Availability Calculations**
   - More reliable weekday calculations with `Timex.weekday/1`
   - Better interval handling for date ranges
   - Improved duration calculations

2. **Booking Operations**
   - More precise time arithmetic for slot generation
   - Better validation of booking time constraints
   - Enhanced timezone handling for global bookings

3. **Recurring Reservations**
   - More flexible date sequence generation
   - Better weekday filtering for business days
   - Improved conflict detection

## Usage Examples

### Basic Operations

```elixir
# Current time
now = Timex.now()

# Date arithmetic
tomorrow = Timex.shift(Timex.today(), days: 1)
next_week = Timex.shift(now, weeks: 1)

# Duration calculations
duration = Timex.diff(end_time, start_time, :minutes)

# Comparisons
case Timex.compare(date1, date2) do
  -1 -> "date1 is before date2"
  0  -> "dates are equal"
  1  -> "date1 is after date2"
end

# Formatting
formatted = Timex.format!(now, "{ISO:Extended}")
```

### Business Logic Examples

```elixir
# Check if a reservation is in the past
def reservation_in_past?(reserved_from) do
  Timex.compare(reserved_from, Timex.now()) == -1
end

# Calculate slot duration
def slot_duration(start_time, end_time) do
  Timex.diff(end_time, start_time, :minutes)
end

# Generate weekdays only
def generate_weekdays(start_date, count) do
  Stream.iterate(start_date, &Timex.shift(&1, days: 1))
  |> Stream.filter(fn date -> Timex.weekday(date) in 1..5 end)
  |> Enum.take(count)
end
```

## Migration Notes

### Breaking Changes

Most changes are internal and don't affect the public API. However, some internal functions now use Timex:

- Weekday calculation now returns 1-7 (Monday-Sunday) consistently
- Date arithmetic uses `Timex.shift/2` instead of `DateTime.add/3`
- Comparisons return -1/0/1 instead of `:lt/:eq/:gt`

### Compatibility

- All existing tests pass with minimal modifications
- Public APIs remain unchanged
- Database interactions are unaffected
- Phoenix/LiveView components work without changes

## Performance Considerations

- Timex is well-optimized and performs comparably to standard Elixir datetime functions
- Timezone operations have minimal overhead when not using timezones
- The `:tzdata` dependency provides efficient timezone database access

## Future Enhancements

With Timex integrated, the project can now easily support:

1. **Multi-timezone Operations**: Handle bookings across different timezones
2. **Advanced Scheduling**: Complex recurring patterns and business rules
3. **Internationalization**: Locale-aware date formatting and parsing
4. **Holiday Support**: Integration with holiday calendars for availability
5. **Business Hours**: Sophisticated business hour calculations

## Troubleshooting

### Common Issues

1. **Time vs DateTime confusion**: Use `DateTime.to_time/1` when you need Time from DateTime
2. **Timezone awareness**: Remember that `Timex.now/0` returns UTC by default
3. **Format strings**: Use Timex format strings like `"{ISO:Extended}"` for formatting

### Getting Help

- [Timex Documentation](https://hexdocs.pm/timex/)
- [Timex GitHub Repository](https://github.com/bitwalker/timex)
- [Getting Started Guide](https://hexdocs.pm/timex/getting-started.html)

## Conclusion

The integration of Timex significantly enhances the project's date/time handling capabilities while maintaining backward compatibility. The library provides a solid foundation for future enhancements requiring sophisticated date/time operations.