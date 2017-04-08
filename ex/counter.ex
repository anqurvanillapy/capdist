##  Counter
##  =======
##
##  Receive pages and page references, send the counts Map and forward the
##  references to Accumulator.

defmodule Counter do
  use GenServer.Behaviour

  def start_link do
    # Name, callback module, and callback arguments (4th argument omitted).
    :gen_server.start_link(__MODULE__, nil, [])
  end

  def deliver_page(pid, ref, page) do
    # No need to wait for a response if cast.
    :gen_server.cast(pid, {:deliver_page, ref, page})
  end

  def init(_args) do
    # Unused argument is underscore-prefixed.
    Parser.request_page(self())
    {:ok, nil}
  end

  def handle_cast({:deliver_page, ref, page}, state) do
    # New a request immediately after receiving one, for reducing latency.
    Parser.request_page(self())

    words = String.split(page)
    # Differently, HashDict is deprecated.
    counts = Enum.reduce(words, Map.new, fn(word, counts) ->
      Map.update(counts, word, 1, &(&1 + 1))
    end)

    Accumulator.deliver_counts(ref, counts)
    {:noreply, state}
  end
end

##  CounterSupervisor
##  =================
##
##  Create and supervise multiple Counters.

defmodule CounterSupervisor do
  use Supervisor.Behaviour

  def start_link(num_counters) do
    # Callback module and callback argument.
    :supervisor.start_link(__MODULE__, num_counters)
  end

  def init(num_counters) do
    workers = Enum.map(1..num_counters, fn(n) ->
      worker(Counter, [], id: "counter#{n}")
    end)

    # Strategy: If the child process terminates, only that one is restarted.
    supervise(workers, strategy: :one_for_one)
  end
end
