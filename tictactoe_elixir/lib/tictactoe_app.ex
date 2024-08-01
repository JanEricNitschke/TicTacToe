defmodule TictactoeApp do
  use Application

  def start(_type, _args) do
    IO.puts(TictactoeElixir.hello())
    # some more stuff
    Supervisor.start_link([], strategy: :one_for_one)
  end
end
