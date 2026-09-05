Chrono := [].{

    # If year, month, or day is not within valid bounds, functions may crash or return nonsense.
    Date :: {
        year : I32,
        month : U8,  # 1 .. 12.
        day : U8,    # 1 .. 31.
    }.{
        new : I32, U8, U8 -> Try(Date, [OutOfRange, ..])
        new = |y, m, d| {
            if m < 1 or m > 12 or d < 1 or d > last_day_of_month(y, m) {
                return Err(OutOfRange)
            }
            Ok({ year: y, month: m, day: d })
        }

        year : Date -> I32
        year = |date| date.year

        month : Date -> U8
        month = |date| date.month

        day : Date -> U8
        day = |date| date.day

        is_eq : Date, Date -> Bool
        is_eq = |a, b| a.year == b.year and a.month == b.month and a.day == b.day

        to_hash : Date, Hasher -> Hasher
        to_hash = |a, hasher| hasher.write_i32(a.year).write_u8(a.month).write_u8(a.day)

        is_lt : Date, Date -> Bool
        is_lt = |a, b| {
            a.year < b.year
                or (a.year == b.year and a.month < b.month)
                or (a.year == b.year and a.month == b.month and a.day < b.day)
        }

        is_lte : Date, Date -> Bool
        is_lte = |a, b| {
            a.year < b.year
                or (a.year == b.year and a.month < b.month)
                or (a.year == b.year and a.month == b.month and a.day <= b.day)
        }

        is_gt : Date, Date -> Bool
        is_gt = |a, b| !is_lte(a, b)

        is_gte : Date, Date -> Bool
        is_gte = |a, b| !is_lt(a, b)

        min : Date, Date -> Date
        min = |a, b| if a <= b { a } else { b }

        max : Date, Date -> Date
        max = |a, b| if a >= b { a } else { b }

        lowest : Date
        lowest = { year: I32.lowest, month: 1, day: 1 }

        highest : Date
        highest = { year: I32.highest, month: 12, day: 31 }

        # For the given date returns the number of days after `1970-01-01`.
        # Uses the proleptic Gregorian calendar.
        to_days : Date -> I64
        to_days = |date| {
            # Implementation is taken from the article chrono-Compatible Low-Level Date Algorithms
            # by Howard Hinnant.

            d = date.day.to_i64()
            m = date.month.to_i64()
            # In our algorithm each year starts with March 1 and ends with February 28 or 29.
            # So if the month is 1 or 2, it's actually the previous year.
            y = date.year.to_i64() - if m <= 2 { 1 } else { 0 }

            # Proleptic Gregorian calendar repeats after 400 years.
            # `era` specifies which 400 years we mean.
            # `era == -1` is for -0400-03-01 .. 0000-02-29.
            # `era == 0`  is for  0000-03-01 .. 0400-02-29.
            # `era == 1`  is for  0400-03-01 .. 0800-02-29.
            # `era == 5`  is for  2000-03-01 .. 2400-02-29.
            era = (y - if y >= 0 { 0 } else { 399 }) / 400

            # Year of era.
            yoe = y - era * 400  # 0 .. 399.
            # Day of year.
            # `m > 2 ? m-3 : m+9` is the month index where March has 0, April has 1, .., February has 11.
            # `d-1` is the day of the current month.
            # `(153 * month_index + 2) / 5` is one of several linear polynomials that
            # returns the total number of days in the months before the month with the given index.
            doy = (153*(m + if m > 2 { -3 } else { 9 }) + 2)/5 + d-1  # 0 .. 365.
            # Day of era.
            doe = yoe*365 + yoe/4 - yoe/100 + doy  # 0 .. 146096.
            # Each era has 146097 days.
            # `era * 146097 + doe` returns the number of days after `0000-03-01`.
            # We subtract `719468` to get the number of days after `1970-01-01`.
            era * 146097 + doe - 719468
        }

        from_days : I64 -> Try(Date, [OutOfRange, ..])
        from_days = |days| {
            # Implementation is taken from the article chrono-Compatible Low-Level Date Algorithms
            # by Howard Hinnant.
            z = days.plus_try(719468) ? |_| OutOfRange

            era = (z - if z >= 0 { 0 } else { 146096 }) / 146097
            doe = (z - era * 146097).to_i32_try() ?? crash "Absurd"  # 0 .. 146096.
            yoe = (doe - doe/1460 + doe/36524 - doe/146096) / 365    # 0 .. 399.
            y = yoe.to_i64() + era*400
            doy = doe - (365*yoe + yoe/4 - yoe/100)    # 0 .. 365.
            mp = (5*doy + 2)/153                       # 0 .. 11.
            d = doy - (153*mp+2)/5 + 1                 # 1 .. 31.
            m = if mp < 10 { mp + 3 } else { mp - 9 }  # 1 .. 12.
            Ok({
                year: (y + if m <= 2 { 1 } else { 0 }).to_i32_try()?,
                month: m.to_u8_try() ?? crash "Absurd",
                day: d.to_u8_try() ?? crash "Absurd",
            })
        }

        add_days : Date, I64 -> Try(Date, [OutOfRange, ..])
        add_days = |date, days| from_days(date.to_days().plus_try(days) ? |_| OutOfRange)

        is_leap : I32 -> Bool
        is_leap = |y| {
            # Implementation is taken from the article chrono-Compatible Low-Level Date Algorithms
            # by Howard Hinnant.

            y % 4 == 0 and (y % 100 != 0 or y % 400 == 0)
        }

        last_day_of_month_common_year : U8 -> U8
        last_day_of_month_common_year = |m| {
            # Implementation is taken from the article chrono-Compatible Low-Level Date Algorithms
            # by Howard Hinnant.

            a : List(U8)
            a = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
            a.get(m.to_u64() - 1) ?? crash "Invalid month"
        }

        last_day_of_month_leap_year : U8 -> U8
        last_day_of_month_leap_year = |m| {
            # Implementation is taken from the article chrono-Compatible Low-Level Date Algorithms
            # by Howard Hinnant.

            a = [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
            a.get(m.to_u64() - 1) ?? crash "Invalid month"
        }

        last_day_of_month : I32, U8 -> U8
        last_day_of_month = |y, m| {
            # Implementation is taken from the article chrono-Compatible Low-Level Date Algorithms
            # by Howard Hinnant.

            if m != 2 or !is_leap(y) { last_day_of_month_common_year(m) } else 29
        }

        to_str : Date -> Str
        to_str = |date| {
            pad_left : Str, U64 -> Str
            pad_left = |str, n| {
                len = str.count_utf8_bytes()
                if len >= n { str } else {
                    Str.repeat("0", n - len).concat(str)
                }
            }
            sign = if date.year < 0 { "-" } else { "" }
            y = date.year.abs().to_str() |> pad_left(4)
            m = date.month.to_str() |> pad_left(2)
            d = date.day.to_str() |> pad_left(2)
            "${sign}${y}-${m}-${d}"
        }

        # Only supports years from 0000 to 9999.
        from_str : Str -> Try(Date, [InvalidDate, ..])
        from_str = |str| {
            bytes = str.to_utf8()

            # The number indicates how many digits have been read so far.
            var $state : [Year(U32), Month(U32), Day(U32)]
            var $state = Year(0)
            var $year = 0
            var $month = 0
            var $day = 0
            for b in bytes {
                if b >= '0' and b <= '9' {
                    digit = b - '0'
                    match $state {
                        Year(i) if i <= 3 => {
                            $state = Year(i + 1)
                            $year = $year * 10 + digit.to_i32()
                        }
                        Month(i) if i <= 1 => {
                            $state = Month(i + 1)
                            $month = $month * 10 + digit
                        }
                        Day(i) if i <= 1 => {
                            $state = Day(i + 1)
                            $day = $day * 10 + digit
                        }
                        _ => { return Err(InvalidDate) }
                    }
                } else if b == '-' {
                    match $state {
                        Year(4) => { $state = Month(0) }
                        Month(2) => { $state = Day(0) }
                        _ => { return Err(InvalidDate) }
                    }
                } else {
                    return Err(InvalidDate)
                }
            }

            if $state != Day(2) or $month < 1 or $month > 12 or $day < 1 or $day > 31 {
                return Err(InvalidDate)
            }
            if $day > last_day_of_month($year, $month) {
                return Err(InvalidDate)
            }
            Ok({
                year: $year,
                month: $month,
                day: $day,
            })
        }

        to_inspect : Date -> Str
        to_inspect = to_str
    }

    Time :: {
        hour : U8,    # 0 .. 23.
        minute : U8,  # 0 .. 59.
        second : U8,  # 0 .. 59.
    }.{
        new : U8, U8, U8 -> Try(Time, [OutOfRange, ..])
        new = |h, m, s| {
            if h > 23 or m > 59 or s > 59 {
                return Err(OutOfRange)
            }
            Ok({ hour: h, minute: m, second: s })
        }

        hour : Time -> U8
        hour = |time| time.hour

        minute : Time -> U8
        minute = |time| time.minute

        second : Time -> U8
        second = |time| time.second

        is_eq : Time, Time -> Bool
        is_eq = |a, b| a.hour == b.hour and a.minute == b.minute and a.second == b.second

        to_hash : Time, Hasher -> Hasher
        to_hash = |a, hasher| hasher.write_u8(a.hour).write_u8(a.minute).write_u8(a.second)

        is_lt : Time, Time -> Bool
        is_lt = |a, b| {
            a.hour < b.hour
                or (a.hour == b.hour and a.minute < b.minute)
                or (a.hour == b.hour and a.minute == b.minute and a.second < b.second)
        }

        is_lte : Time, Time -> Bool
        is_lte = |a, b| {
            a.hour < b.hour
                or (a.hour == b.hour and a.minute < b.minute)
                or (a.hour == b.hour and a.minute == b.minute and a.second <= b.second)
        }

        is_gt : Time, Time -> Bool
        is_gt = |a, b| !is_lte(a, b)

        is_gte : Time, Time -> Bool
        is_gte = |a, b| !is_lt(a, b)

        min : Time, Time -> Time
        min = |a, b| if a <= b { a } else { b }

        max : Time, Time -> Time
        max = |a, b| if a >= b { a } else { b }

        lowest : Time
        lowest = { hour: 0, minute: 0, second: 0 }

        highest : Time
        highest = { hour: 23, minute: 59, second: 59 }

        to_seconds : Time -> U32
        to_seconds = |t| t.hour.to_u32() * 3600 + t.minute.to_u32() * 60 + t.second.to_u32()

        from_seconds : U32 -> Try(Time, [OutOfRange, ..])
        from_seconds = |secs| {
            h = secs / 3600
            m = (secs % 3600) / 60
            s = (secs % 3600) % 60
            if h > 23 { return Err(OutOfRange) }
            Ok({
                hour: h.to_u8_try() ?? crash "Absurd",
                minute: m.to_u8_try() ?? crash "Absurd",
                second: s.to_u8_try() ?? crash "Absurd",
            })
        }

        add_seconds : Time, U32 -> Try(Time, [OutOfRange, ..])
        add_seconds = |t, secs| from_seconds(t.to_seconds().plus_try(secs) ? |_| OutOfRange)

        to_str : Time -> Str
        to_str = |t| {
            pad_left : Str, U64 -> Str
            pad_left = |str, n| {
                len = str.count_utf8_bytes()
                if len >= n { str } else {
                    Str.repeat("0", n - len).concat(str)
                }
            }
            h = t.hour.to_str() |> pad_left(2)
            m = t.minute.to_str() |> pad_left(2)
            s = t.second.to_str() |> pad_left(2)
            "${h}:${m}:${s}"
        }

        # Parses `h:mm:ss` or `hh:mm:ss`.
        # Leap seconds are not supported.
        from_str : Str -> Try(Time, [InvalidTime, ..])
        from_str = |str| {
            bytes = str.to_utf8()

            var $state : [Hour(U32), Minute(U32), Second(U32)]
            var $state = Hour(0)
            var $hour = 0
            var $minute = 0
            var $second = 0
            for b in bytes {
                if b >= '0' and b <= '9' {
                    digit = b - '0'
                    match $state {
                        Hour(i) if i <= 1 => {
                            $state = Hour(i + 1)
                            $hour = $hour * 10 + digit
                        }
                        Minute(i) if i <= 1 => {
                            $state = Minute(i + 1)
                            $minute = $minute * 10 + digit
                        }
                        Second(i) if i <= 1 => {
                            $state = Second(i + 1)
                            $second = $second * 10 + digit
                        }
                        _ => { return Err(InvalidTime) }
                    }
                } else if b == ':' {
                    match $state {
                        Hour(1) | Hour(2) => { $state = Minute(0) }
                        Minute(2) => { $state = Second(0) }
                        _ => { return Err(InvalidTime) }
                    }
                } else {
                    return Err(InvalidTime)
                }
            }

            if $state != Second(2) or $hour >= 24 or $minute >= 60 or $second >= 60 {
                return Err(InvalidTime)
            }
            Ok({
                hour: $hour,
                minute: $minute,
                second: $second,
            })
        }

        to_inspect : Time -> Str
        to_inspect = to_str
    }

    DateTime := {
        date: Date,
        time: Time,
    }.{
        is_eq : DateTime, DateTime -> Bool
        is_eq = |a, b| a.date == b.date and a.time == b.time

        to_hash : DateTime, Hasher -> Hasher
        to_hash = |a, hasher| a.time.to_hash(a.date.to_hash(hasher))

        is_lt : DateTime, DateTime -> Bool
        is_lt = |a, b| {
            a.date < b.date
                or (a.date == b.date and a.time < b.time)
        }

        is_lte : DateTime, DateTime -> Bool
        is_lte = |a, b| {
            a.date < b.date
                or (a.date == b.date and a.time <= b.time)
        }

        is_gt : DateTime, DateTime -> Bool
        is_gt = |a, b| !is_lte(a, b)

        is_gte : DateTime, DateTime -> Bool
        is_gte = |a, b| !is_lt(a, b)

        min : DateTime, DateTime -> DateTime
        min = |a, b| if a <= b { a } else { b }

        max : DateTime, DateTime -> DateTime
        max = |a, b| if a >= b { a } else { b }

        lowest : DateTime
        lowest = { date: Date.lowest, time: Time.lowest }

        highest : DateTime
        highest = { date: Date.highest, time: Time.highest }

        to_seconds : DateTime -> I64
        to_seconds = |dt| dt.date.to_days() * 86_400 + dt.time.to_seconds().to_i64()

        from_seconds : I64 -> Try(DateTime, [OutOfRange, ..])
        from_seconds = |secs| {
            days = secs.div_floor_by(86_400)
            secs_since_midnight = secs.mod_by(86_400).to_u32_try() ?? crash "Absurd"
            Ok({
                date: Date.from_days(days)?,
                time: Time.from_seconds(secs_since_midnight) ?? crash "Absurd",
            })
        }

        add_seconds : DateTime, I64 -> Try(DateTime, [OutOfRange, ..])
        add_seconds = |dt, secs| DateTime.from_seconds(dt.to_seconds().plus_try(secs) ? |_| OutOfRange)

        to_str : DateTime -> Str
        to_str = |dt| "${dt.date.to_str()} ${dt.time.to_str()}"

        # Only supports years from 0000 to 9999.
        # Leap seconds are not supported.
        from_str : Str -> Try(DateTime, [InvalidDateTime, ..])
        from_str = |str| {
            parts = str.split_first(" ") ? |_| InvalidDateTime
            date = Date.from_str(parts.before) ? |_| InvalidDateTime
            time = Time.from_str(parts.after) ? |_| InvalidDateTime
            Ok({ date, time })
        }

        to_inspect : DateTime -> Str
        to_inspect = to_str
    }

}

# Round trip.
expect {
    date = Date.{ year: 2026, month: 9, day: 4 }
    Date.from_days(date.to_days()) == Ok(date)
}

# Zero.
expect {
    date = Date.{ year: 1970, month: 1, day: 1 }
    date.to_days() == 0
}

expect {
    zero = Date.{ year: 1970, month: 1, day: 1 }
    test_up : Date, I64, U32 -> Bool
    test_up = |date, number_of_days, steps| {
        if steps == 0 { True } else {
            new_date = if date.day < Date.last_day_of_month(date.year, date.month) {
                { ..date, day: date.day + 1 }
            } else if date.month < 12 {
                { ..date, month: date.month + 1, day: 1 }
            } else {
                { year: date.year + 1, month: 1, day: 1 }
            }
            new_number_of_days = number_of_days + 1

            new_date.to_days() == new_number_of_days
                and Date.from_days(new_number_of_days) == Ok(new_date)
                and test_up(new_date, new_number_of_days, steps - 1)
        }
    }

    test_down : Date, I64, U32 -> Bool
    test_down = |date, number_of_days, steps| {
        if steps == 0 { True } else {
            new_date = if date.day > 1 {
                { ..date, day: date.day - 1 }
            } else if date.month > 1 {
                { ..date, month: date.month - 1, day: Date.last_day_of_month(date.year, date.month - 1) }
            } else {
                { year: date.year - 1, month: 12, day: 31 }
            }
            new_number_of_days = number_of_days - 1

            new_date.to_days() == new_number_of_days
                and Date.from_days(new_number_of_days) == Ok(new_date)
                and test_down(new_date, new_number_of_days, steps - 1)
        }
    }

    test_up(zero, 0, 10_000_000) and test_down(zero, 0, 10_000_000)
}

# Biggest number of days which is too small.
expect {
    Date.from_days(-784353015834) == Err(OutOfRange)
}

# Lowest number of days which can be converted to date.
expect {
    days = -784353015833
    Date.from_days(days)? == Date.lowest and days == Date.lowest.to_days()
}

# Smallest number of days which is too high.
expect {
    Date.from_days(784351576777) == Err(OutOfRange)
}

# Highest number of days which can be converted to date.
expect {
    days = 784351576776
    Date.from_days(days)? == Date.highest and days == Date.highest.to_days()
}

# Positive seconds.
expect {
    dt = DateTime.{ date: { year: 1970, month: 1, day: 10 }, time: Time.highest }
    seconds = 86_400 * 10 - 1
    dt.to_seconds() == seconds and DateTime.from_seconds(seconds)? == dt
}

# Negative seconds.
expect {
    dt = DateTime.{ date: { year: 1969, month: 12, day: 30 }, time: Time.highest }
    seconds = -86_400 - 1
    dt.to_seconds() == seconds and DateTime.from_seconds(seconds)? == dt
}

expect {
    test : DateTime, I64, U64 -> Bool
    test = |dt, seconds, steps| {
        if steps == 0 { True } else {

            new_dt = if dt.time != Time.highest {
                { ..dt, time: dt.time.add_seconds(1) ?? crash "Absurd" }
            } else {
                { date: dt.date.add_days(1) ?? crash "Absurd", time: Time.lowest }
            }
            new_seconds = seconds + 1

            new_dt.to_seconds() == new_seconds
                and DateTime.from_seconds(new_seconds) == Ok(new_dt)
                and test(new_dt, new_seconds, steps - 1)
        }
    }

    # Date -0100-01-01 is -756_052 days from 1970-01-01.
    # Date 1960-01-01 is -3_653 days from 1970-01-01.
    test({ date: { year: -100, month: 1, day: 1 }, time: Time.lowest }, -756_052 * 86_400, 70_000 * 86_400)
        and test({ date: { year: 1960, month: 1, day: 1 }, time: Time.lowest }, -3_653 * 86_400, 7_000 * 86_400)
}
