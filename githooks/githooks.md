all git hooks must be copied in ./.git/hooks 
OR, 
if git version >= 2.9.0, then config variable core.hooksPath should point at ./githooks
$git config core.hooksPath ./githooks

NOTE:
if hook isn't running, try
chmod +x .git/hooks/<git-hook-name>
