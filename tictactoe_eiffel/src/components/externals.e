class EXTERNALS

inherit
    UNISTD_EXTERNALS
        redefine
            sleep
        end

feature {ANY} -- Access
    copy (other: like Current)
        external "built_in"
        end

    is_equal (other: like Current): BOOLEAN
            -- Is `other' attached to an object considered equal to current object?
        do
            Result := True
        end

    sleep (seconds: NATURAL_32): NATURAL_32
            -- Sleep for the given number of seconds.
        do
            Result := Precursor(seconds)
        end

end -- class EXTERNALS
