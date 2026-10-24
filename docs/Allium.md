**Confirmed by running the probes:**
- **P1, quality start for relievers.** A pitcher with 0 games started and 18 outs gets a quality start. The quality-start test never looks at `pitGamesStarted`.
- **P2, impossible stat lines score points.** A line with 1 hit, 1 double and 1 home run scores 6 points instead of being rejected. Singles are floored at 0, so the extra-base hits count and nothing is subtracted.
- **P3 and P4, `generateDraftOrder` drops picks.**
  - When there are fewer picks than teams, it returns no picks at all.
  - With 7 picks across 3 teams, it returns 6.
  - It can't happen today, because picks always equal roster size times teams. It will break as soon as rosters vary by team or a draft has to resume.
- **P5, `experimental_snake` is unfair with an odd number of teams.** With 3 teams over 4 rounds, the pick-number totals are 27, 24 and 27, so the middle team gets a systematic advantage. P6 shows it is fair with 4 teams.

**Found by reading the code (the spec has an invariant or a "CODE:" note for each):**
- **Snapshots aren't tied to first pitch.** `snapshot lineups --on-date D` records each team's current lineup whenever you run it. Run it after games start, or after they finish, and the snapshot is a lineup chosen with results in view. The module's "lineup at game start" contract is enforced nowhere.
- **Snapshots aren't actually write-once.** "Snapshot already exists" means "any row exists for this team and game". A team with an empty lineup writes zero rows, so the next run snapshots whatever lineup is there by then.
- **`activateLeague` only checks that the league is in draft status.** A league with no draft run, empty rosters and empty lineups can go active.
- **`EndDraft` marks a draft complete with short rosters.** That contradicts the claim in `Draft/Machine.hs` that a completed draft always satisfies roster limits.
- **Roster slots aren't checked against positions.**
  - `validateRoster` and `validateLineup` check counts and duplicates only.
  - The feed calls every pitcher "P", so starter and reliever slots mean nothing.
  - Two-way players can't be drafted.
- **Scoring doesn't check whether a game is final.** Box scores are upserted whatever the game's status, so a score computed mid-game is partial and isn't marked as such.
- **Postponed games can fall out of the scoring period.** Scoring filters by the game's current date, so a game moved outside the period silently drops out.
- **Matchups are paired in database row order.** There is no schedule.
- **Rankings and lineups have smaller problems:**
  - `replaceRanking` doesn't check the league or draft status.
  - A lineup can only be set as "the first N roster entries per slot", so owners have no way to bench a player.
  - The CLI has no ranking or lineup command at all.
- **The auto-draft can't resume, and its fallback isn't "best available".** A league with any pick already made is refused. When a team's ranking runs out, it gets the lowest internal id, which is arbitrary.

The spec ends with 13 open questions for you to rule on, such as whether a quality start requires a start and whether two-way players are meant to be undraftable.

**To add the probes to your test suite (any machine with the repo; your flake's GHC should build it):**
1. Save `ProbeSpec.hs` as `test/Pelotero/ProbeSpec.hs`.
2. Add `Pelotero.ProbeSpec` to the `other-modules` list of the `pelotero-test` test suite in `pelotero-engine.cabal`, on the line after `Pelotero.MatchupSpec`.
3. Replace `test/Spec.hs` with this:

```haskell
module Main (main) where

import Test.Hspec

import qualified Pelotero.Domain.DraftSpec        as DomainDraft
import qualified Pelotero.Domain.PlayerSpec       as DomainPlayer
import qualified Pelotero.Domain.PositionSpec     as Position
import qualified Pelotero.Domain.RosterSpec       as Roster
import qualified Pelotero.Domain.ScoringSpec      as Scoring
import qualified Pelotero.Domain.EligibilitySpec  as Eligibility
import qualified Pelotero.Draft.MachineSpec       as DraftMachine
import qualified Pelotero.Draft.RunSpec           as DraftRun
import qualified Pelotero.DraftSpec               as Draft
import qualified Pelotero.Effects.PlayersSpec     as PlayersEff
import qualified Pelotero.Effects.RandomSpec      as RandomEff
import qualified Pelotero.League.SetupSpec        as LeagueSetup
import qualified Pelotero.MatchupSpec             as Matchup
import qualified Pelotero.ProbeSpec               as Probe
import qualified Pelotero.MLB.ConvertSpec         as Convert
import qualified Pelotero.Provider.ExternalIdSpec as ExternalId
import qualified Pelotero.ScoreSpec               as Score
import qualified Pelotero.Sync.BoxscoresSpec      as SyncBoxscores
import qualified Pelotero.Sync.PlayersSpec        as SyncPlayers
import qualified Pelotero.Sync.ScheduleSpec       as SyncSchedule

main :: IO ()
main = hspec $ do
  Probe.spec
  Position.spec
  Roster.spec
  Scoring.spec
  DomainDraft.spec
  DomainPlayer.spec
  Eligibility.spec

  ExternalId.spec

  Convert.spec

  Draft.spec
  DraftMachine.spec
  DraftRun.spec

  PlayersEff.spec
  RandomEff.spec

  Score.spec
  Matchup.spec
  LeagueSetup.spec

  SyncPlayers.spec
  SyncSchedule.spec
  SyncBoxscores.spec
```

The probes describe how the code should behave, so they will fail until each bug is fixed. That's intentional: once you've ruled on an open question, the matching probe becomes a regression test. The snapshot-timing and empty-lineup snapshot bugs touch the database, so they need integration tests rather than probes.

I've left those changes uncommitted on purpose. You never asked me to commit or push anything to pelotero-engine, and this copy of the repo was cloned without push access.

The uncommitted changes are the probe file `test/Pelotero/ProbeSpec.hs`, the edited `test/Spec.hs`, and one added line in `pelotero-engine.cabal`. I already gave you all three to add yourself, and five of the six probes fail until you fix the bugs they test. If you do want them on a branch, tell me which one and I'll attach the repo with push access and commit there.

Two existing tests conflict with the probes, and each conflict needs a ruling from you:

- **`generateDraftOrder (serpentine)` has a test called "truncates partial trailing rounds"**, which says P4's behaviour is intended.
  - If truncation is the rule, P3 and P4 are wrong and should be deleted.
  - If drafts should keep every pick, that test is asserting the bug and should go.
  - It's harmless today because picks always equal roster size times teams. It decides what happens once rosters vary by team.
- **The property "a completed draft fills every slot of every roster exactly" passes**, but it can't be covering `EndDraft`.
  - The test "EndDraft from Drafting transitions to Complete" shows an early end also counts as complete. So the property only holds for drafts that finish with their last pick.
  - The fix is either to make `EndDraft` produce a separate abandoned state, or to stop calling that property a guarantee.

The quality-start test "credits a quality start: 18 outs (6.0 IP), 3 ER" passes because it leaves `pitGamesStarted` unset. Once P1 is fixed, a stat line without that field gets no quality start, so the test needs `pitGamesStarted = Just 1`.

Which of these do you want fixed in code: P1 (quality start needs a start), P2 (reject inconsistent lines at sync, or score them), P3 and P4 (keep every pick, or truncate), P5 (make the snake fair for odd counts, or allow only even counts), and `EndDraft`? Give me rulings and I'll send the full changed modules and updated tests.