-- Remember that this needs to be enabled for each DB connection.
PRAGMA foreign_keys = ON;
-- Note that sqlx, the library I'm using to manage SQL connections, is going to turn this on automatically.
-- I don't think that's been released quite yet, though.

INSERT INTO Servers (ID)
VALUES (695085554940772432); -- The Arrival

INSERT INTO Games (ServerID)
VALUES (695085554940772432); -- The Arrival's Game
-- SELECT last_insert_rowid()
-- to get the autoincrement ID

-- Since this is for testing with no other data in the DB, we already know the autoincrement IDs
-- But, in reality we'll need to fetch those after each insert.
-- Or, we'll fetch what would be the next one and use it to make all the succeeding ones
-- for a bulk insert. We'll see.
INSERT INTO Channels (ID, GameID, DefaultGameMode, ControlState, RoundsToRetain)
VALUES (695085937520017478, 1, 0, 1, 2); -- #main-rp, <snip>, unordered, on, 2 rounds

INSERT INTO Players (ID, GameID)
VALUES (97171464851042304, 1); -- Monadic Cat#4158

INSERT INTO Rounds (ChannelID, StartTime, GameMode, RemainingTurns)
VALUES (695085937520017478, "2020-07-16 05:05:05.005", 0, 4); -- #main-rp, <time>, unordered, 4 turns
-- SELECT last_insert_rowid()
-- to get the autoincrement ID

INSERT INTO RoundPlayers (PlayerID, GameID, RoundID, UsedTurns)
VALUES (97171464851042304, 1, 1, 0);

INSERT INTO Turns (PlayerID, GameID, RoundID)
VALUES (97171464851042304, 1, 1);
