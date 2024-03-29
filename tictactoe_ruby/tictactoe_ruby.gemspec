# frozen_string_literal: true

require_relative "lib/tictactoe_ruby/version"
require "rake"

Gem::Specification.new do |spec|
  spec.name = "tictactoe_ruby"
  spec.version = TictactoeRuby::VERSION
  spec.authors = ["Jan-Eric Nitschke"]
  spec.email = ["janericnitschke@gmail.com"]

  spec.summary = "Tictactoe written in ruby."
  spec.description = "Tictactoe written in ruby."
  spec.homepage = "https://github.com/JanEricNitschke/TicTacToe/tree/main/tictactoe_ruby"
  spec.license = "MIT"
  spec.required_ruby_version = ">= 3.1.0"

  spec.metadata["allowed_push_host"] = "https://example.com'"

  spec.metadata["homepage_uri"] = spec.homepage
  spec.metadata["source_code_uri"] = "https://github.com/JanEricNitschke/TicTacToe/tree/main/tictactoe_ruby"
  spec.metadata["changelog_uri"] = "https://github.com/JanEricNitschke/TicTacToe/tree/main/tictactoe_ruby"
  spec.metadata["rubygems_mfa_required"] = "true"

  # Specify which files should be added to the gem when it is released.
  # The `git ls-files -z` loads the files in the RubyGem that have been added into git.
  spec.files = FileList["lib/**/*",
                        "bin/*",
                        "test/*"].to_a
  spec.require_paths = ["lib"]

  # Uncomment to register a new dependency of your gem
  # spec.add_dependency "example-gem", "~> 1.0"

  # For more information and examples about making a new gem, check out our
  # guide at: https://bundler.io/guides/creating_gem.html
end
