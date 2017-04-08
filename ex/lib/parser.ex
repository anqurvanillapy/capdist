##  Parser
##  ======
##
##  This maintains two states which are pending (a Keyword that stores a set of
##  the references of unprocessed pages which have been sent to the Counters),
##  and xml_parser (an actor that utilizes Erlang's xmerl to parse the Wikipedia
##  dump).

defmodule Parser do
  use GenServer

  def start_link(filename) do
    :gen_server.start_link({:global, :wc_parser}, __MODULE__, filename, [])
  end

  def request_page(wid) do
    # A worker (Counter) send a request and its ID.
    :gen_server.cast({:global, :wc_parser}, {:request_page, wid})
  end

  def processed(ref) do
    # Notify the page has been processed.
    :gen_server.cast({:global, :wc_parser}, {:processed, ref})
  end

  def init(filename) do
    # ListDict is deprecated, use Keyword instead (Keyword is essentially a
    # List, and could be manipulated by the List module). Therefore, Keyword is
    # basically *not efficient*.
    xml_parser = Pages.start_link(filename)
    {:ok, {Keyword.new, xml_parser}}
  end

  def handle_cast({:request_page, wid}, {pending, xml_parser}) do
    # Get the next available page from XML parser.
    new_pending = deliver_page(wid, pending, Pages.next(xml_parser))
    {:noreply, {new_pending, xml_parser}}
  end

  def handle_cast({:processed, ref}, {pending, xml_parser}) do
    # Delete processed pages.
    new_pending = Keyword.delete(pending, ref)
    {:noreply, {new_pending, xml_parser}}
  end

  defp deliver_page(wid, pending, page) when is_nil(page) do
    # `when' is a guard clause, which makes the function available only when the
    # value of the boolean expression is true.
    if Enum.empty?(pending) do
      pending # nop
    else
      {ref, prev_page} = List.last(pending)
      Counter.deliver_page(wid, ref, prev_page)
      Keyword.put(Keyword.delete(pending, ref), ref, prev_page)
    end
  end

  defp deliver_page(wid, pending, page) do
    # make_ref returns an *almost* unique reference, which might re-occur after
    # approximately 2 ** 82 calls.
    ref = make_ref()
    Counter.deliver_page(wid, ref, page)
    Keyword.put(pending, ref, page)
  end
end

##  ParserSupervisor
##  ================
##
##  Create and supervise multiple the Parser.

defmodule ParserSupervisor do
  use Supervisor

  def start_link(filename) do
    :supervisor.start_link(__MODULE__, filename)
  end

  def init(filename) do
    workers = [worker(Parser, [filename])]
    supervise(workers, strategy: :one_for_one)
  end
end
