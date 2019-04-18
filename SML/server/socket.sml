signature ASIO =
sig
    signature Socket =
    sig
        type sock
        val socket: unit -> sock
        val bind: sock -> int
        val connect: sock -> int
        val listen: sock int -> int
        val accept: sock -> sock option
        val close: sock -> unit
    end
end
