# Deploy the TRISK demo to Hugging Face Spaces

A **free, public showcase** of the TRISK Shiny app on Hugging Face Spaces
(Docker SDK). Runs on synthetic data, no auth — separate from the hardened
bank build in the repo root.

## Why Hugging Face

- **Free** CPU Space with **16 GB RAM** — fits the app's 2–4 GB footprint.
- **Docker-native** — reuses the project's pinned multi-stage build.
- **Always-warm** persistent URL (sleeps after ~48h idle, wakes on visit).

## One-time setup

1. Create a Hugging Face account: https://huggingface.co/join
2. Create a **write** token: https://huggingface.co/settings/tokens
3. Install git-lfs if needed: `brew install git-lfs`

## Deploy

```bash
export HF_TOKEN=hf_xxxxxxxx        # your write token
export HF_USERNAME=your-handle     # your HF username
export SPACE_NAME=trisk-demo       # optional

bash deploy/hf/push-to-hf.sh
```

The script creates the Space, assembles a self-contained Space repo
(Dockerfile + app + scenario data), and pushes it. HF then builds the image.

- **Build logs:** `https://huggingface.co/spaces/<user>/trisk-demo?logs=build`
- **Live app:** `https://<user>-trisk-demo.hf.space`
- First build takes **~15–30 min** (R packages compile). Later pushes are faster.

## Files

| File | Role |
|------|------|
| `Dockerfile.hf`   | Demo image — binds port 7860, no Caddy/TLS, world-writable output |
| `space-README.md` | HF Space card (becomes the Space's `README.md`; YAML front matter configures it) |
| `.gitattributes`  | Git-LFS tracking for the 59 MB `scenarios.csv` |
| `push-to-hf.sh`   | Assemble + create Space + push |

## Updating the live demo

Re-run `bash deploy/hf/push-to-hf.sh` after any change to `app/`. It force-pushes
the latest tree and HF rebuilds automatically.

## Access control (optional)

This demo is **open by default** (standard for HF showcases). To gate it:

- **Simplest:** in the Space's **Settings → make it Private**, then share with
  specific HF accounts.
- **Password gate on a public Space:** add Caddy basic-auth in front of Shiny
  (two-process container). Ask and this can be wired into `Dockerfile.hf`.

## Notes / limits

- Audit logs and exports write to ephemeral container storage — they reset on
  rebuild/restart. That's fine for a demo (no persistent volume needed).
- HF free CPU Spaces are not for production data. Real deployments use the
  hardened root `docker-compose.yml` (TLS + Basic Auth + read-only fs).
