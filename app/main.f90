program main
  use iso_fortran_env, only: int8, int16
  use iso_c_binding
  use libarrow_glib, only: garrow_int8_array_builder_new, garrow_int8_array_builder_append_values, &
    garrow_array_builder_finish, garrow_int16_array_builder_new, garrow_int16_array_builder_append_values
  
  use libarrow_glib, only: garrow_field_new, garrow_int8_data_type_new, garrow_int16_data_type_new, &
    garrow_schema_new, garrow_schema_add_field, garrow_field_get_name, garrow_field_get_data_type, &
    garrow_field_to_string, garrow_schema_to_string

  use libarrow_glib, only: garrow_record_batch_new, garrow_record_batch_to_string

  use g, only: g_list_alloc, g_list_append, g_list_first, g_free, g_object_unref

  use util, only: c_f_str_ptr
  implicit none

  type(c_ptr) :: int8builder, int16builder
  type(c_ptr) :: days, months, years
  type(c_ptr) :: field_days, field_months, field_years
  type(c_ptr) :: fields_list, columns_list
  type(c_ptr) :: schema 
  type(c_ptr) :: rbatch
  integer(c_int) :: status 
  type(c_ptr) :: error

  int8builder = garrow_int8_array_builder_new()
  block 
    integer(int8), target :: days_raw(5)
    integer(c_int64_t) :: n
    integer(c_bool), target :: is_valids(5)

    days_raw = [1, 12, 17, 23, 28]
    is_valids = .true.
    n = size(days_raw, dim=1, kind=c_int64_t)

    status = garrow_int8_array_builder_append_values(int8builder, c_loc(days_raw), n, &
      c_loc(is_valids), n, error)

    days = garrow_array_builder_finish(int8builder, error)
  end block
  
  block 
    integer(int8), target :: months_raw(5)
    integer(c_int64_t) :: n
    integer(c_bool), target :: is_valids(5)

    months_raw = [1, 3, 5, 7, 1]
    is_valids = .true.
    n = size(months_raw, dim=1, kind=c_int64_t)

    status = garrow_int8_array_builder_append_values(int8builder, c_loc(months_raw), n, &
    c_loc(is_valids), n, error)
    months = garrow_array_builder_finish(int8builder, error)
  end block
  call g_object_unref(int8builder)

  int16builder = garrow_int16_array_builder_new()
  block 
    integer(int16), target :: years_raw(5)
    integer(c_int64_t) :: n
    integer(c_bool), target :: is_valids(5)

    years_raw = [1990, 2000, 1995, 2000, 1995]
    is_valids = .true.
    n = size(years_raw, dim=1, kind=c_int64_t)

    status = garrow_int16_array_builder_append_values(int16builder, c_loc(years_raw), n, &
      c_loc(is_valids), n, error)

    years = garrow_array_builder_finish(int16builder, error)
  end block
  call g_object_unref(int16builder)

  field_days = garrow_field_new('days'//c_null_char, garrow_int8_data_type_new())
  field_months = garrow_field_new('months'//c_null_char, garrow_int8_data_type_new())
  field_years = garrow_field_new('years'//c_null_char, garrow_int16_data_type_new())
  
  fields_list = c_null_ptr
  fields_list = g_list_append(fields_list, field_days)
  fields_list = g_list_append(fields_list, field_months)
  fields_list = g_list_append(fields_list, field_years)

  schema = garrow_schema_new(fields_list)

  block 
    character(len=:), allocatable :: str
    call c_f_str_ptr(garrow_schema_to_string(schema), str)
    print*, str
  end block

  ! With the schema and Arrays full of data, we can make our RecordBatch! Here,
  ! each column is internally contiguous. This is in opposition to Tables, which we'll
  ! see next.
  
  columns_list = c_null_ptr
  columns_list = g_list_append(columns_list, days)
  columns_list = g_list_append(columns_list, months)
  columns_list = g_list_append(columns_list, years)
  ! The RecordBatch needs the schema, length for columns, which all must match,
  ! and the actual data itself.
  rbatch = garrow_record_batch_new(schema, 5_c_int32_t, columns_list, error)

  block 
    character(len=:), allocatable :: str
    call c_f_str_ptr(garrow_record_batch_to_string(rbatch, error), str)
    print*, str
  end block
  
end program main
