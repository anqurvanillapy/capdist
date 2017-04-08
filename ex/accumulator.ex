##  Accumulator
##  ===========
##
##  This maintains two states which are totals (a Map with counting results) and
##  processed_pages (a MapSet of the references of processed pages).

defmodule Accumulator do
  use GenServer.Behaviour

  def start_link do
    # Register a global name.
    :gen_server.start_link({:global, :wc_accumulator}, __MODULE__,
      {Map.new, MapSet.new}, [])
  end

  def deliver_counts(ref, counts) do
    :gen_server.cast({:global, :wc_accumulator}, {:deliver_counts, ref, counts})
  end

  def handle_cast({:deliver_counts, ref, counts}, {totals, processed_pages}) do
    if MapSet.member?(processed_pages, ref) do
      # If the page has been counted, no need to accumulate.
      {:noreply, {totals, processed_pages}}
    else
      _totals = Map.merge(totals, counts, fn(_k, v1, v2) -> v1 + v2 end)
      _processed_pages = MapSet.put(processed_pages, ref)
      Parser.processed(ref)
      {:noreply, {_totals, _processed_pages}}
    end
  end
end

##  AccumulatorSupervisor
##  =====================
##
##  Create and supervise multiple the Accumulator.

defmodule AccumulatorSupervisor do
  use Supervisor.Behaviour

  def start_link do
    :supervisor.start_link(__MODULE__, []) 
  end

  def init(_args) do
    workers = [worker(Accumulator, [])]
    supervise(workers, strategy: :one_for_one)
  end
end
