use anyhow::{anyhow, Context, Result};
use std::path::PathBuf;
use std::process::Command;

pub struct WorktreeManager {
    repo_root: PathBuf,
}

impl WorktreeManager {
    /// Create a new WorktreeManager for the current directory
    pub fn new() -> Result<Self> {
        let output = Command::new("git")
            .args(["rev-parse", "--show-toplevel"])
            .output()
            .context("Failed to run git rev-parse")?;

        if !output.status.success() {
            return Err(anyhow!("Not in a git repository"));
        }

        let root = String::from_utf8(output.stdout)
            .context("Invalid UTF-8 in git output")?
            .trim()
            .to_string();

        Ok(Self {
            repo_root: PathBuf::from(root),
        })
    }

    /// Get the worktree path for a branch
    /// Convention: ../project-name-branch-name
    pub fn worktree_path(&self, branch: &str) -> PathBuf {
        let project_name = self
            .repo_root
            .file_name()
            .and_then(|s| s.to_str())
            .unwrap_or("project");

        // Sanitize branch name (replace / with -)
        let safe_branch = branch.replace('/', "-");

        self.repo_root
            .parent()
            .unwrap_or(&self.repo_root)
            .join(format!("{}-{}", project_name, safe_branch))
    }

    /// Check if a worktree exists for a branch
    pub fn worktree_exists(&self, branch: &str) -> Result<bool> {
        let worktree_path = self.worktree_path(branch);

        let output = Command::new("git")
            .args(["worktree", "list", "--porcelain"])
            .current_dir(&self.repo_root)
            .output()
            .context("Failed to list worktrees")?;

        let list = String::from_utf8(output.stdout).context("Invalid UTF-8 in worktree list")?;

        Ok(list.contains(&worktree_path.to_string_lossy().to_string()))
    }

    /// Check if a branch exists (locally or remotely)
    fn branch_exists(&self, branch: &str) -> Result<bool> {
        // Check local branch
        let local = Command::new("git")
            .args(["rev-parse", "--verify", branch])
            .current_dir(&self.repo_root)
            .output()?
            .status
            .success();

        if local {
            return Ok(true);
        }

        // Check remote branch
        let remote = Command::new("git")
            .args(["rev-parse", "--verify", &format!("origin/{}", branch)])
            .current_dir(&self.repo_root)
            .output()?
            .status
            .success();

        Ok(remote)
    }

    /// Create a worktree for a branch
    pub fn create_worktree(&self, branch: &str) -> Result<PathBuf> {
        let worktree_path = self.worktree_path(branch);

        if worktree_path.exists() {
            // Check if it's already a valid worktree
            if self.worktree_exists(branch)? {
                return Ok(worktree_path);
            }
            return Err(anyhow!(
                "Path {} exists but is not a worktree",
                worktree_path.display()
            ));
        }

        let branch_exists = self.branch_exists(branch)?;

        let status = if branch_exists {
            // Branch exists, just add worktree
            Command::new("git")
                .args(["worktree", "add", worktree_path.to_str().unwrap(), branch])
                .current_dir(&self.repo_root)
                .status()
                .context("Failed to create worktree")?
        } else {
            // Create new branch from current HEAD
            Command::new("git")
                .args([
                    "worktree",
                    "add",
                    "-b",
                    branch,
                    worktree_path.to_str().unwrap(),
                ])
                .current_dir(&self.repo_root)
                .status()
                .context("Failed to create worktree with new branch")?
        };

        if !status.success() {
            return Err(anyhow!("Failed to create worktree for branch {}", branch));
        }

        Ok(worktree_path)
    }

    /// Get or create worktree, returning the path to cd into
    pub fn ensure_worktree(&self, branch: &str) -> Result<PathBuf> {
        if self.worktree_exists(branch)? {
            Ok(self.worktree_path(branch))
        } else {
            self.create_worktree(branch)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_worktree_path() {
        let manager = WorktreeManager {
            repo_root: PathBuf::from("/Users/test/Projects/myproject"),
        };

        assert_eq!(
            manager.worktree_path("feature/api"),
            PathBuf::from("/Users/test/Projects/myproject-feature-api")
        );

        assert_eq!(
            manager.worktree_path("main"),
            PathBuf::from("/Users/test/Projects/myproject-main")
        );
    }
}
