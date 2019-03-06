Sequel.migration do
  up do
    alter_table(:claims) do
      add_column :deleted_at, DateTime
    end
  end

  down do
    alter_table(:claims) do
      drop_column :deleted_at
    end
  end
end
