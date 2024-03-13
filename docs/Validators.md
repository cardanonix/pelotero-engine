# ⚾ Validators ⚾
a suite of functions for validating, processing, and managing baseball team lineups and rosters within a specific configuration context utilizing Haskell's strong type system and functional programming paradigms to offer robust utilities for ensuring roster compliance with league rules, detecting discrepancies, and performing lineup validations. Below is a summary of each function and its purpose:

# Functions
- extractNameFromPath: Extracts the filename from a given file path, useful for processing or displaying file names without their directory paths.

- processConfigResults: Takes a configuration and a list of files (with their contents parsed as team rosters), testing each lineup against the configuration rules and reporting the results.

- testLineup: Tests a single team's lineup against the configuration rules, printing the current lineup and the validation result.

- validateLineup: Validates a team's lineup against a set of configuration rules, checking for discrepancies in player positions and ensuring unique player IDs.

- validateAndPrintLineup: Similar to validateLineup, but specifically designed for IO operations, printing out detailed information about lineup validation results.

- findPlayer: Searches for a player by ID within a list of official players and a list of available player IDs, returning the player if found.

- maxPossibleTeams and maxPossibleTeamsForPosition: Calculate the maximum number of teams that can be formed from an official roster, based on position-specific limits and the total available players for each position.

- isLineupValid and getRosterValidationErrors: Provide a boolean validity check for a lineup and a way to retrieve specific roster validation errors, respectively.

- lineupHasUniquePlayers and hasUniqueRosterPlayers: Check for duplicate player IDs within a lineup or roster, ensuring player uniqueness.

- lookupPlayerInRoster: Determines if a player ID is present within a specific roster.

- getRosterDiscrepancies: Identifies discrepancies in a roster against draft limits, reporting positions with too many players.

- validateRoster: Validates a roster against configuration draft limits and checks for unique player IDs.

- hasValidPositions: Checks if a player has valid position assignments, ensuring they're eligible to play in their designated spots.

- lookupPlayerInOfficialRoster and validatePlayerId: Functions to ensure player IDs are valid and exist within an official roster, facilitating integrity checks for player data.

- intToText, getUniquePlayerIdsLineup, and getLineupDiscrepancies: Utility functions for converting integers to text, extracting unique player IDs from a lineup, and identifying discrepancies in lineup configurations against league rules.

- validatePositionCount and totalPlayersInLineup: Assist in validating the count of players per position within a lineup and calculating the total number of players.

- validateCurrentLineup: Provides a high-level validation of the current lineup, combining positional checks with player uniqueness validations.

- findPlayerPosition and batterOrPitcher: Determine a player's position within a lineup and classify them as either a batter or a pitcher, aiding in role-specific validations and point calculations.

# Dependencies

- base >=4.11 && <4.18
- random
- memory
- filepath
- http-conduit
- time
- bytestring
- aeson
- vector
- unordered-containers
- text
- containers
- cryptohash-sha256
- crypton
- scientific
- directory
- debug-trace-var
- bytestring
- aeson
- text
- containers
- async
- cassava
```

# Contribute
Feel free to fork, improve, create pull requests, report bugs, or request new features.

Made with ❤️ by Harry Pray IV.