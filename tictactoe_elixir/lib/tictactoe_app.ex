defmodule TictactoeApp do
  use Application

  def parse_args(args) do
    {opts, _, _} = OptionParser.parse(args, strict: [x_strength: :integer, o_strength: :integer])

    x_strength = Keyword.get(opts, :x_strength, 0)
    o_strength = Keyword.get(opts, :o_strength, 0)

    {x_strength, o_strength}
  end

  def start(_type, _args) do
    {x_strength, o_strength} = parse_args(System.argv())
    TictactoeElixir.play(x_strength, o_strength)
    # some more stuff
    Supervisor.start_link([], strategy: :one_for_one)
  end
end
