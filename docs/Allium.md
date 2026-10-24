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