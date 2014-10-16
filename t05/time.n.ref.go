package main

import "fmt"

const (
	// The unsigned zero year for internal calculations.
	// Must be 1 mod 400, and times before it will not compute correctly,
	// but otherwise can be changed at will.
	absoluteZeroYear = -292277022399

	// The year of the zero Time.
	// Assumed by the unixToInternal computation below.
	internalYear = 1

	// The year of the zero Unix time.
	unixYear = 1970

        secondsPerDay = 24*3600
	// Offsets to convert between internal and absolute or Unix times.
	absoluteToInternal int64 = (absoluteZeroYear - internalYear) * 365.2425 * secondsPerDay
	internalToAbsolute       = -absoluteToInternal

	unixToInternal int64 = (1969*365 + 1969/4 - 1969/100 + 1969/400) * secondsPerDay
	internalToUnix int64 = -unixToInternal
)

func main() {
  fmt.Println(absoluteZeroYear)
  fmt.Println(internalYear)
  fmt.Println(unixYear)
  fmt.Println(secondsPerDay)
  fmt.Println(absoluteToInternal)
  fmt.Println(internalToAbsolute)
  fmt.Println(unixToInternal)
  fmt.Println(internalToUnix)
}
