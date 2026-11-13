# Post-Flight Verification (CoVe)

Referenced by `.claude/skills/lit-review/SKILL.md`. Run it before handing a literature review, or any output with citations or numbers, to the user.

1. **Extract claims.** Each cited paper, each paraphrased finding, each number and each negative claim ("no prior work studies Y") is one claim. Give each an ID (C1, C2, …).
2. **Write a verification question for each.** Make it specific: "Does Smith (2019, *JEL*) Section 3 report X? Is the venue correct?"
3. **Spawn `claim-verifier`** (`.claude/agents/claim-verifier.md`) with the `Agent` tool in a fresh context. Pass the claims table, the questions and the source pointers (URLs, DOIs, local paths). **Do not pass the draft**: independence is the point.
4. **Reconcile.**
   - PASS: attach a green Post-Flight block.
   - PARTIAL: fix what the verifier corrected, and flag what stays unconfirmed (in the text and in the BibTeX `note` field).
   - FAIL: remove or rewrite each contradicted claim using the verifier's evidence, then re-verify the rewritten claims.

## Post-Flight block format

Append to the report, collapsed:

```markdown
<details>
<summary>Post-Flight Verification: PASS | PARTIAL | FAIL (date, N claims)</summary>

| ID | Claim (short) | Verdict | Action taken |
|---|---|---|---|

**Still unconfirmed:** …
**Verifier:** claim-verifier, fresh context; draft not shared.
</details>
```
