# Claude Code defensively clamps to 256-color whenever $TMUX is set,
# even when tmux is truecolor-capable (anthropics/claude-code#36785).
# Must be exported from the shell, not settings.json, since the clamp
# runs before settings env injection.
set -gx CLAUDE_CODE_TMUX_TRUECOLOR 1
