interface Props {
  value: string | number;
  label: string;
}

export function StatTile({ value, label }: Props) {
  return (
    <div className="stat-tile">
      <div className="value">{value}</div>
      <div className="label">{label}</div>
    </div>
  );
}
