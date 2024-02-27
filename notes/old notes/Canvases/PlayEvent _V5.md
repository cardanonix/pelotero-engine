
# Haskell GHC 8.10.7 Code Summary

## Types

-   `GameId`: A newtype wrapper around a `String` to represent a unique game identifier.
-   `GameStatus`: Represents the status of a game (`InProgress` or `Finished`).
-   `Scorecard`: Contains the game identifier, a list of play events, and the game status.
-   `HalfInning`: Represents the top or bottom half of an inning.
-   `PlayEventType`: Enumerates various types of play events that can occur in a baseball game.
-   `PlayEvent`: Represents a single play event, containing event type, inning, half-inning, batter, pitcher, team IDs, runner movement, runs scored, RBIs, outs, and errors.

## Random Data Generation

-   `generateRandomGameId`: Generates a random `GameId`.
-   `generateRandomPlayEventType`: Generates a random `PlayEventType`.
-   `generateRandomInning`: Generates a random inning number (1-9).
-   `generateRandomHalfInning`: Generates a random `HalfInning` (either `Top` or `Bottom`).
-   `generateRandomBatter`: Selects a random batter from a list of MLB players.
-   `generateRandomPlay`: Generates a random play event using a list of MLB players and two team IDs.

## Scorecard Functions

-   `emptyScorecard`: Creates an empty scorecard with a given game identifier and `InProgress` status.
-   `recordPlay`: Adds a play event to a scorecard.
-   `simulateHalfInning`: Simulates a half inning of a baseball game, generating random play events and recording them in a scorecard.