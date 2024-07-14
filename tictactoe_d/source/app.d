void main(string[] args)
{
    import game : play;
    import std.getopt : getopt;

    int xStrength = 0;
    int oStrength = 0;

    getopt(
        args,
        "X-strength", &xStrength,
        "O-strength", &oStrength
    );

    play(xStrength, oStrength);
}
