use anyhow::{Context, Result};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::PathBuf;

#[derive(Debug, Deserialize, Serialize, Default)]
pub struct Config {
    #[serde(default)]
    pub defaults: Defaults,
    #[serde(default)]
    pub profiles: HashMap<String, Profile>,
}

#[derive(Debug, Deserialize, Serialize)]
pub struct Defaults {
    #[serde(default = "default_claude_path")]
    pub claude_path: String,
    #[serde(default = "default_fps")]
    pub fps: u32,
}

impl Default for Defaults {
    fn default() -> Self {
        Self {
            claude_path: default_claude_path(),
            fps: default_fps(),
        }
    }
}

fn default_claude_path() -> String {
    "~/.claude/local/claude".to_string()
}

fn default_fps() -> u32 {
    60
}

#[derive(Debug, Deserialize, Serialize, Default, Clone)]
pub struct Profile {
    /// API endpoint URL
    pub endpoint: Option<String>,
    /// Direct API key (not recommended, prefer api_key_env)
    pub api_key: Option<String>,
    /// Environment variable name containing API key
    pub api_key_env: Option<String>,
}

impl Config {
    /// Load config from standard locations
    /// Search order: ~/.config/cc/profiles.toml, ~/.cc.toml
    pub fn load() -> Result<Self> {
        let config_paths = [
            dirs::config_dir().map(|p| p.join("cc/profiles.toml")),
            dirs::home_dir().map(|p| p.join(".cc.toml")),
        ];

        for path in config_paths.into_iter().flatten() {
            if path.exists() {
                let content = std::fs::read_to_string(&path)
                    .with_context(|| format!("Failed to read config file: {}", path.display()))?;
                let config: Config = toml::from_str(&content)
                    .with_context(|| format!("Failed to parse config file: {}", path.display()))?;
                return Ok(config);
            }
        }

        // Return default config if no file found
        Ok(Config::default())
    }

    /// Get a profile by name, returns None if not found
    pub fn get_profile(&self, name: &str) -> Option<&Profile> {
        self.profiles.get(name)
    }

    /// Expand the claude path (handles ~)
    pub fn claude_path(&self) -> PathBuf {
        let expanded = shellexpand::tilde(&self.defaults.claude_path);
        PathBuf::from(expanded.as_ref())
    }
}

impl Profile {
    /// Build environment variables for this profile
    pub fn env_vars(&self) -> Vec<(String, String)> {
        let mut vars = Vec::new();

        if let Some(endpoint) = &self.endpoint {
            vars.push(("ANTHROPIC_BASE_URL".to_string(), endpoint.clone()));
        }

        // Try api_key_env first, then direct api_key
        if let Some(env_name) = &self.api_key_env {
            if let Ok(key) = std::env::var(env_name) {
                vars.push(("ANTHROPIC_API_KEY".to_string(), key));
            }
        } else if let Some(key) = &self.api_key {
            vars.push(("ANTHROPIC_API_KEY".to_string(), key.clone()));
        }

        vars
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_default_config() {
        let config = Config::default();
        assert_eq!(config.defaults.fps, 60);
        assert!(config.defaults.claude_path.contains("claude"));
    }

    #[test]
    fn test_parse_config() {
        let toml = r#"
[defaults]
claude_path = "/usr/local/bin/claude"
fps = 30

[profiles.work]
endpoint = "https://api.work.example.com"
api_key_env = "WORK_KEY"

[profiles.local]
endpoint = "http://localhost:8080"
api_key = "test-key"
"#;
        let config: Config = toml::from_str(toml).unwrap();
        assert_eq!(config.defaults.fps, 30);
        assert_eq!(config.profiles.len(), 2);
        assert!(config.profiles.contains_key("work"));
        assert!(config.profiles.contains_key("local"));
    }
}
