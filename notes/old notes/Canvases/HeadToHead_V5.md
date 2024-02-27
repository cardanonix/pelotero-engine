
# Haskell GHC 8.10.7 Code Summary

## Types

-   `Week`: A type alias for an `Int` representing a week.
-   `FantasyLeagueId`: A newtype wrapper around a `String` to represent a unique fantasy league identifier.
-   `FantasyLeague`: Represents a fantasy league, containing league ID, managers, roster size, active lineup size, reserve size, injured list size, and current date.
-   `ManagerId`: A newtype wrapper around a `String` to represent a unique manager identifier.
-   `Record`: A type alias for a tuple of two `Int`s representing a manager's record (wins, losses).
-   `Manager`: Represents a manager, containing manager ID, fantasy team name, fantasy league ID, draft picks, active lineup, reserve players, injured list, dual threat status (Ohtani rule), and record.
-   `PointVals`: Contains the point values for batting and pitching events.
-   `BattingPointVals`: Represents the point values for each batting event.
-   `PitchingPointVals`: Represents the point values for each pitching event.

## Functions

-   `setBattingPointVals`: Maps batting point values to the corresponding batting stats.
-   `setPitchingPointVals`: Maps pitching point values to the corresponding pitching stats.
-   `leaguePointVals`: Default point values for the fantasy league.
-   `calculatePoints`: Calculates the points earned by a player, given their stats and point values.
-   `sumPitchingPoints`: Sums the total points earned by a pitcher, given their stats and point values.
-   `sumBattingPoints`: Sums the total points earned by a batter, given their stats and point values.
-   `emptyBattingPoints`: An empty `BattingPointVals` object with all values set to 0.0.
-   `emptyPitchingPoints`: An empty `PitchingPointVals` object with all values set to 0.0.