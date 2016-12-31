defmodule Tanuki.Mixfile do
  use Mix.Project

  def project do
    [apps_path: "apps",
     build_embedded: Mix.env == :prod,
     start_permanent: Mix.env == :prod,
     deps: deps]
  end

  # Dependencies listed here are available only for this project and cannot
  # be accessed from applications inside the apps folder
  defp deps do
    [{:distillery, "~> 1.0"},
     {:mix_erlang_tasks, github: "heroku/mix-erlang-tasks", branch: "umbrella_support"}]
  end
end