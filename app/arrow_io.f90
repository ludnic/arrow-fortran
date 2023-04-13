program arrow_io
  use libarrow_glib
  use parquet_glib
  use iso_c_binding
  use iso_fortran_env, only: int8, int16
  use g
  implicit none 

  type(c_ptr) :: infile, error, rbatch, ipc_reader
  type(c_ptr) :: outfile, ipc_writer
  integer(c_int) :: status

  ! Generate initial files for each format with a helper function -- don't worry,
  ! we'll also write a table in this example.
  call gen_initial_file()

  print*, "Done creating initial files."
  ! First, we have to set up a ReadableFile object, which just lets us point our
  ! readers to the right data on disk. We'll be reusing this object, and rebinding
  ! it to multiple files throughout the example.
  infile = garrow_file_input_stream_new('test_in.arrow'//c_null_char, error)
  ! Open up the file with the IPC features of the library, gives us a reader object.
  ! ipc_reader = garrow_record_batch_stream_reader_new(infile, error)
  ipc_reader = garrow_record_batch_file_reader_new(infile, error)

  ! Using the reader, we can read Record Batches. Note that this is specific to IPC;
  ! for other formats, we focus on Tables, but here, RecordBatches are used.
  rbatch = garrow_record_batch_file_reader_read_record_batch(ipc_reader, 0_c_int, error)
  ! rbatch = garrow_record_batch_reader_read_next(ipc_reader, error)

  print*, "Done reading. "
  ! Just like with input, we get an object for the output file.
  outfile = garrow_file_output_stream_new('test_out.arrow'//c_null_char, 0_c_int, error)
  ! Set up a writer with the output file -- and the schema! We're defining everything
  ! here, loading to fire.
  ipc_writer = garrow_record_batch_stream_writer_new(outfile, garrow_record_batch_get_schema(rbatch), error)
  ! Write the record batch.
  status = garrow_record_batch_writer_write_table(ipc_writer, rbatch, error)
  ! Specifically for IPC, the writer needs to be explicitly closed.
  status = garrow_record_batch_writer_close(ipc_writer, error)

contains 

  subroutine gen_initial_file()
    ! Make a couple 8-bit integer arrays and a 16-bit integer array -- just like
    ! basic Arrow example.
    type(c_ptr) :: int8builder, int16builder
    integer(int8), target :: days_raw(5), months_raw(5)
    integer(int16), target :: years_raw(5)
    logical(c_bool), target :: is_valids(5)
    type(c_ptr) :: error
    integer(c_int) :: status
    type(c_ptr) :: days, months, years 
    type(c_ptr) :: list
    type(c_ptr) :: schema 
    type(c_ptr) :: table
    type(c_ptr) :: outfile, ipc_writer, parquet_writer

    int8builder = garrow_int8_array_builder_new()
    is_valids = .true.

    days_raw = [1, 12, 17, 23, 28]
    status = garrow_int8_array_builder_append_values(int8builder, c_loc(days_raw), &
      5_c_int64_t, c_loc(is_valids), 5_c_int64_t, error)
    days = garrow_array_builder_finish(int8builder, error)

    months_raw = [1, 3, 5, 7, 1]
    status = garrow_int8_array_builder_append_values(int8builder, c_loc(months_raw), &
    5_c_int64_t, c_loc(is_valids), 5_c_int64_t, error)
    months = garrow_array_builder_finish(int8builder, error)

    int16builder = garrow_int16_array_builder_new()
    years_raw = [1990, 2000, 1995, 2000, 1995]
    status = garrow_int16_array_builder_append_values(int16builder, c_loc(years_raw), &
      5_c_int64_t, c_loc(is_valids), 5_c_int64_t, error)
    years = garrow_array_builder_finish(int16builder, error)

    ! Make a schema to initialize the Table with
    block 
      type(c_ptr) field_day, field_month, field_year

      field_day = garrow_field_new('day'//c_null_char, garrow_int8_data_type_new())
      field_month = garrow_field_new('month'//c_null_char, garrow_int8_data_type_new())
      field_year = garrow_field_new('year'//c_null_char, garrow_int16_data_type_new())

      list=c_null_ptr
      list = g_list_append(list, field_day)
      list = g_list_append(list, field_month)
      list = g_list_append(list, field_year)

      schema = garrow_schema_new(list)
    end block 

    ! Get a vector of our Arrays
    list = c_null_ptr
    list = g_list_append(list, days)
    list = g_list_append(list, months)
    list = g_list_append(list, years)

    ! With the schema and data, create a Table
    table = garrow_table_new_values(schema, list, error)

    ! Write out test files in IPC, CSV, and Parquet for the example to use.
    outfile = garrow_file_output_stream_new('test_in.arrow'//c_null_char, 0_c_int, error)
    ipc_writer = garrow_record_batch_stream_writer_new(outfile, schema, error)
    status = garrow_record_batch_writer_write_table(ipc_writer, table, error)
    status = garrow_record_batch_writer_close(ipc_writer, error)
    
    parquet_writer = gparquet_arrow_file_writer_new_path(schema, 'test_in.parquet'//c_null_char, c_null_ptr, error)
    status = gparquet_arrow_file_writer_write_table(parquet_writer, table, 5_c_int64_t, error)

  end subroutine gen_initial_file
end program arrow_io